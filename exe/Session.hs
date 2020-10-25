{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- | A persistent version of the Ghci session, encoding lots of semantics on top.
--   Not suitable for calling multithreaded.
module Session
  ( Session
  , enableEval
  , withSession
  , sessionStart
  , sessionReload
  , sessionExecAsync
  )
where

import Ghcid
  ( Ghci
  , exec
  , execStream
  , interrupt
  , process
  , quit
  , reload
  , showModules
  , showPaths
  , startGhci
  )
import Ghcid.Types
  ( EvalResult (..)
  , Load (..)
  , Severity (..)
  , Stream (..)
  , isLoadConfig
  )
import System.FilePath ((</>))

import qualified Control.Concurrent.Extra as Concurrent
import qualified Control.Exception as Exception
import qualified Data.List.Extra as List
import qualified Data.String as String
import qualified Ghcid.Escape as Escape
import qualified Ghcid.Util as Util
import qualified System.Console.ANSI as Ansi
import qualified System.IO.Extra as IO
import qualified System.Process as Process
import qualified System.Time.Extra as System.Time

data Session = Session
  { ghci :: IORef (Maybe Ghci) -- ^ The Ghci session, or Nothing if there is none
  , command :: IORef (Maybe (String, [String])) -- ^ The last command passed to sessionStart, setup operations
  , warnings :: IORef [Load] -- ^ The warnings from the last load
  , curdir :: IORef FilePath -- ^ The current working directory
  , running :: Concurrent.Var Bool -- ^ Am I actively running an async command
  , withThread :: Concurrent.ThreadId -- ^ Thread that called withSession
  , allowEval :: Bool  -- ^ Is the allow-eval flag set?
  }

enableEval :: Session -> Session
enableEval s = s{ allowEval = True }

debugShutdown :: String -> IO ()
debugShutdown x = when False $ print ("DEBUG SHUTDOWN" :: String, x)

-- | The function 'withSession' expects to be run on the main thread,
--   but the inner function will not. This ensures Ctrl-C is handled
--   properly and any spawned Ghci processes will be aborted.
withSession :: (Session -> IO a) -> IO a
withSession f = do
  ghci <- newIORef Nothing
  command <- newIORef Nothing
  warnings <- newIORef []
  curdir <- newIORef "."
  running <- Concurrent.newVar False
  debugShutdown "Starting session"
  withThread <- Concurrent.myThreadId
  let allowEval = False
  f Session{..} `Exception.finally` do
    debugShutdown "Start finally"
    Concurrent.modifyVar_ running $ const $ pure False
    whenJustM (readIORef ghci) \v -> do
      writeIORef ghci Nothing
      debugShutdown "Calling kill"
      kill v
    debugShutdown "Finish finally"


-- | Kill. Wait just long enough to ensure you've done the job, but not to see the results.
kill :: Ghci -> IO ()
kill ghci = Util.ignored do
  void $ System.Time.timeout 5 do
    debugShutdown "Before quit"
    Util.ignored $ quit ghci
    debugShutdown "After quit"
  debugShutdown "Before terminateProcess"
  Util.ignored $ Process.terminateProcess $ process ghci
  debugShutdown "After terminateProcess"
  -- Ctrl-C after a tests keeps the cursor hidden,
  -- `setSGR []`didn't seem to be enough
  -- See: https://github.com/ndmitchell/ghcid/issues/254
  Ansi.showCursor

loadedModules :: [Load] -> [FilePath]
loadedModules = List.nubOrd . map loadFile . filter (not . isLoadConfig)

qualify :: FilePath -> [Load] -> [Load]
qualify dir xs = [x{loadFile = dir </> loadFile x} | x <- xs]

-- | Spawn a new Ghci process at a given command line. Returns the load messages, plus
--   the list of files that were observed (both those loaded and those that failed to load).
sessionStart :: Session -> String -> [String] -> IO ([Load], [FilePath])
sessionStart Session{ ghci = ghciIORef, ..} cmd setup = do
  Concurrent.modifyVar_ running $ const $ pure False
  writeIORef command $ Just (cmd, setup)

  -- cleanup any old instances
  whenJustM (readIORef ghciIORef) \v -> do
    writeIORef ghciIORef Nothing
    void $ Concurrent.forkIO $ kill v

  -- start the new
  Util.outStrLn $ "Loading " <> cmd <> " ..."
  (v, messages0) <- Exception.mask \unmask -> do
    (v, messages) <- unmask $ startGhci cmd Nothing $ const Util.outStrLn
    writeIORef ghciIORef $ Just v
    pure (v, messages)

  -- do whatever preparation was requested
  void $ exec v $ String.unlines setup

  -- deal with current directory
  (dir, _) <- showPaths v
  writeIORef curdir dir
  let messages1 = qualify dir messages0

  let loaded = loadedModules messages1
  evals <- performEvals v allowEval loaded

  -- install a handler
  void $ Concurrent.forkIO do
    code <- Process.waitForProcess $ process v
    whenJustM (readIORef ghciIORef) \ghci ->
      when (ghci == v) do
        -- give anyone reading from the stream a chance to throw first
        System.Time.sleep 0.3
        Concurrent.throwTo withThread $ Exception.ErrorCall $ "Command \"" <> cmd <> "\" exited unexpectedly with " <> show code

  -- handle what the process returned
  let messages2 = mapMaybe tidyMessage messages1
  writeIORef warnings $ getWarnings messages2
  pure (messages2 <> evals, loaded)


getWarnings :: [Load] -> [Load]
getWarnings messages = [m | m@Message{..} <- messages, loadSeverity == Warning]


-- | Call 'sessionStart' at the previous command.
sessionRestart :: Session -> IO ([Load], [FilePath])
sessionRestart session@Session{..} = do
  Just (cmd, setup) <- readIORef command
  sessionStart session cmd setup


performEvals :: Ghci -> Bool -> [FilePath] -> IO [Load]
performEvals _ False _ = pure []
performEvals ghci True reloaded = do
  cmds <- mapM getCommands reloaded
  join <$> forM cmds \(file, cmds') ->
    forM cmds' \(num, cmd) -> do
      ref <- newIORef []
      execStream ghci cmd \_ resp -> modifyIORef ref (resp :)
      resp <- String.unlines . reverse <$> readIORef ref
      pure $ Eval $ EvalResult file (num, 1) cmd resp


getCommands :: FilePath -> IO (FilePath, [(Int, String)])
getCommands fp = do
  ls <- IO.readFileUTF8' fp
  pure (fp, splitCommands $ List.zipFrom 1 $ String.lines ls)

splitCommands :: [(Int, String)] -> [(Int, String)]
splitCommands [] = []
splitCommands ((num, line) : ls)
  | isCommand line =
      let (cmds, xs) = List.span (isCommand . snd) ls
       in (num, String.unwords $ fmap (drop $ length commandPrefix) $ line : fmap snd cmds) : splitCommands xs
  | isMultilineCommandPrefix line =
      let (cmds, xs) = break (isMultilineCommandSuffix . snd) ls
       in (num, String.unlines (wrapGhciMultiline (fmap snd cmds))) : splitCommands (drop 1 xs)
  | otherwise = splitCommands ls

isCommand :: String -> Bool
isCommand = isPrefixOf commandPrefix

commandPrefix :: String
commandPrefix = "-- $> "

isMultilineCommandPrefix :: String -> Bool
isMultilineCommandPrefix = (==) multilineCommandPrefix

multilineCommandPrefix :: String
multilineCommandPrefix = "{- $>"

isMultilineCommandSuffix :: String -> Bool
isMultilineCommandSuffix = (==) multilineCommandSuffix

multilineCommandSuffix :: String
multilineCommandSuffix = "<$ -}"

wrapGhciMultiline :: [String] -> [String]
wrapGhciMultiline xs = [":{"] <> xs <> [":}"]

-- | Reload, returning the same information as 'sessionStart'. In particular, any
--   information that GHCi doesn't repeat (warnings from loaded modules) will be
--   added back in.
sessionReload :: Session -> IO ([Load], [FilePath], [FilePath])
sessionReload session@Session{ ghci = ghciIORef, ..} = do
  -- kill anything async, set stuck if you didn't succeed
  old <- Concurrent.modifyVar running \b -> pure (False, b)
  stuck <- if not old then pure False else do
    Just ghci <- readIORef ghciIORef
    fmap isNothing $ System.Time.timeout 5 $ interrupt ghci

  if stuck then
    (\(messages,loaded) -> (messages, loaded, loaded)) <$> sessionRestart session
  else do
    -- actually reload
    Just ghci <- readIORef ghciIORef
    dir <- readIORef curdir
    messages0 <- mapMaybe tidyMessage . qualify dir <$> reload ghci
    loaded <- map ((dir </>) . snd) <$> showModules ghci
    let reloaded = loadedModules messages0
    warn <- readIORef warnings
    evals <- performEvals ghci allowEval reloaded

    -- only keep old warnings from files that are still loaded, but did not reload
    let validWarn w = loadFile w `elem` loaded && loadFile w `notElem` reloaded
    -- newest warnings always go first, so the file you hit save on most recently has warnings first
    let messages = messages0 <> filter validWarn warn

    writeIORef warnings $ getWarnings messages
    pure (messages <> evals, List.nubOrd (loaded <> reloaded), reloaded)


-- | Run an exec operation asynchronously. Should not be a @:reload@ or similar.
--   Will be automatically aborted if it takes too long. Only fires done if not aborted.
--   Argument to done is the final stderr line.
sessionExecAsync :: Session -> String -> (String -> IO ()) -> IO ()
sessionExecAsync Session{ ghci = ghciIORef, .. } cmd done = do
  Just ghci <- readIORef ghciIORef
  stderrIORef <- newIORef ""
  Concurrent.modifyVar_ running $ const $ pure True
  caller <- Concurrent.myThreadId
  void $ flip Concurrent.forkFinally (either (Concurrent.throwTo caller) (const pass)) do
    execStream ghci cmd \strm msg ->
      when (msg /= "*** Exception: ExitSuccess") do
        when (strm == Stderr) $ writeIORef stderrIORef msg
        Util.outStrLn msg
    old <- Concurrent.modifyVar running \b -> pure (False, b)
    -- don't fire Done if someone interrupted us
    stderr <- readIORef stderrIORef
    when old $ done stderr


-- | Ignore entirely pointless messages and remove unnecessary lines.
tidyMessage :: Load -> Maybe Load
tidyMessage Message{loadSeverity=Warning, loadMessage=[_,x]}
  | Escape.unescape x == "    -O conflicts with --interactive; -O Util.ignored." = Nothing
tidyMessage m@Message{..}
  = Just m{loadMessage = filter (\x -> not $ any (`isPrefixOf` Escape.unescape x) bad) loadMessage}
  where bad =
          [ "      except perhaps to import instances from"
          , "    To import instances alone, use: import "
          ]
tidyMessage x = Just x
