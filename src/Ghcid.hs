{-# LANGUAGE RecordWildCards #-}

-- | Library for spawning and working with Ghci sessions.
module Ghcid
  ( Ghci
  , startGhci
  , interrupt
  , process
  , execStream
  , showModules
  , showPaths
  , reload
  , exec
  , quit
  )
where

import Data.Unique (Unique)
import Ghcid.Types
  ( GhciError (..)
  , Load (..)
  , Stream (..)
  , isLoading
  )
import Relude.Extra.Enum (next)

import qualified Control.Concurrent.Extra as Concurrent.Extra
import qualified Control.Exception as Exception
import qualified Control.Exception.Extra as Exception.Extra
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.Extra as List.Extra
import qualified Data.Maybe as Maybe
import qualified Data.String as String
import qualified Data.Unique as Unique
import qualified Ghcid.Parser as Parser
import qualified Ghcid.Util as Util
import qualified System.Console.CmdArgs.Verbosity as Verbosity
import qualified System.IO as IO
import qualified System.IO.Error as IO.Error
import qualified System.Process as Process

-- | A GHCi session. Created with 'startGhci', closed with 'stopGhci'.
--
--   The interactions with a 'Ghci' session must all occur single-threaded,
--   or an error will be raised. The only exception is 'interrupt', which aborts
--   a running computation, or does nothing if no computation is running.
data Ghci = Ghci
  { ghciProcess :: Process.ProcessHandle
  , ghciInterrupt :: IO ()
  , ghciExec :: String -> (Stream -> String -> IO ()) -> IO ()
  , ghciUnique :: Unique
  }

instance Eq Ghci where
  (==) = (==) `on` ghciUnique

withCreateProc
  :: Process.CreateProcess
  -> (  Maybe Handle
     -> Maybe Handle
     -> Maybe Handle
     -> Process.ProcessHandle
     -> IO a
     )
  -> IO a
withCreateProc processConfig f = do
  let start = Process.createProcess processConfig

  let stop (_, _, _, processHandle) =
        Util.ignored $ Process.terminateProcess processHandle

  let with (stdinHandle, stdoutHandle, stderrHandle, processHandle) =
        f stdinHandle stdoutHandle stderrHandle processHandle

  Exception.bracketOnError start stop with

-- | Start GHCi by running the described process, returning  the result of the
--   initial loading.
--   If you do not call 'stopGhci' then the underlying process may be leaked.
--   The callback will be given the messages produced while loading, useful if
--   invoking something like "cabal repl" which might compile dependent packages
--   before really loading.
--
--   To create a 'CreateProcess' use the functions in "System.Process",
--   particularly 'System.Process.shell' and 'System.Process.proc'.
--
--   @since 0.6.11
startGhciProcess
  :: Process.CreateProcess
  -> (Stream -> String -> IO ())
  -> IO (Ghci, [Load])
startGhciProcess procConfig0 echo0 = do
    let procConfig = procConfig0
          { Process.std_in = Process.CreatePipe
          , Process.std_out = Process.CreatePipe
          , Process.std_err = Process.CreatePipe
          , Process.create_group = True
          }
    withCreateProc procConfig \maybeInp maybeOut maybeErr ghciProcess -> do
        let inp = Maybe.fromJust maybeInp
        let out = Maybe.fromJust maybeOut
        let err = Maybe.fromJust maybeErr

        IO.hSetBuffering out IO.LineBuffering
        IO.hSetBuffering err IO.LineBuffering
        IO.hSetBuffering inp IO.LineBuffering
        let writeInp x = do
                Verbosity.whenLoud $ Util.outStrLn $ "%STDIN: " <> x
                IO.hPutStrLn inp x

        -- Some programs (e.g. stack) might use stdin before starting ghci (see
        -- #57) Send them an empty line
        IO.hPutStrLn inp ""

        -- I'd like the GHCi prompt to go away, but that's not possible, so I set
        -- it to a special string and filter that out.
        let ghcid_prefix = "#~GHCID-START~#"
        let removePrefix = Util.dropPrefixRepeatedly ghcid_prefix

        -- At various points I need to ensure everything the user is waiting for
        -- has completed So I send messages on stdout/stderr and wait for them to
        -- arrive
        syncCount <- Concurrent.Extra.newVar (0 :: Int)
        let syncReplay = do
                i <- Concurrent.Extra.readVar syncCount
                -- useful to avoid overloaded strings by showing the
                -- ['a','b','c'] form, see #109
                let showStr xs = "[" <> intercalate "," (map show xs) <> "]"
                let msg = "#~GHCID-FINISH-" <> show i <> "~#"
                -- Prepend a leading \n to try and avoid junk already on stdout,
                -- e.g. https://github.com/ndmitchell/ghcid/issues/291
                writeInp $ "\nINTERNAL_GHCID.putStrLn " <> showStr msg <> "\n" <>
                           "INTERNAL_GHCID.hPutStrLn INTERNAL_GHCID.stderr " <> showStr msg
                pure $ List.isInfixOf msg
        let syncFresh = do
                Concurrent.Extra.modifyVar_ syncCount $ pure . next
                syncReplay

        -- Consume from a stream until EOF (pure Nothing) or some predicate
        -- returns Just
        let consume :: Stream -> (String -> IO (Maybe a)) -> IO (Either (Maybe String) a)
            consume name finish = do
                let h = if name == Stdout then out else err
                flip fix Nothing \rec oldMsg -> do
                    el <- Exception.Extra.tryBool IO.Error.isEOFError $ IO.hGetLine h
                    case el of
                        Left _ -> pure $ Left oldMsg
                        Right l -> do
                            Verbosity.whenLoud $ Util.outStrLn $ "%" <> fmap Char.toUpper (show name) <> ": " <> l
                            let msg = removePrefix l
                            res <- finish msg
                            case res of
                                Nothing -> rec $ Just msg
                                Just a -> pure $ Right a

        let consume2 :: String -> (Stream -> String -> IO (Maybe a)) -> IO (a,a)
            consume2 msg finish = do
                -- fetch the operations in different threads as hGetLine may block
                -- and can't be aborted by async exceptions, see #154
                res1Action <- Concurrent.Extra.onceFork $ consume Stdout (finish Stdout)
                res2Action <- Concurrent.Extra.onceFork $ consume Stderr (finish Stderr)
                res1 <- res1Action
                res2 <- res2Action
                let raise msg' err' = Exception.throwIO $ case Process.cmdspec procConfig of
                        Process.ShellCommand cmd ->
                          UnexpectedExit cmd msg' err'
                        Process.RawCommand exe args ->
                          UnexpectedExit (String.unwords (exe:args)) msg err'
                case (res1, res2) of
                    (Right v1, Right v2) -> pure (v1, v2)
                    (_, Left err') -> raise msg err'
                    (_, Right _) -> raise msg Nothing

        -- held while interrupting, and briefly held when starting an exec
        -- ensures exec values queue up behind an ongoing interrupt and no two
        -- interrupts run at once
        isInterrupting <- Concurrent.Extra.newLock

        -- is anyone running running an exec statement, ensure only one person
        -- talks to ghci at a time
        isRunning <- Concurrent.Extra.newLock

        let ghciExec command echo = do
                Concurrent.Extra.withLock isInterrupting pass
                res <- Concurrent.Extra.withLockTry isRunning do
                    writeInp command
                    stop <- syncFresh
                    void $ consume2 command \strm s ->
                        if stop s then
                          pure $ Just ()
                        else do
                          void $ echo strm s
                          pure Nothing
                whenNothing res $
                    fail "Ghcid.exec, computation is already running, must be used single-threaded"

        let ghciInterrupt = Concurrent.Extra.withLock isInterrupting $
                whenNothingM (Concurrent.Extra.withLockTry isRunning pass) do
                    Verbosity.whenLoud $ Util.outStrLn "%INTERRUPT"
                    Process.interruptProcessGroupOf ghciProcess
                    -- let the person running ghciExec finish, since their sync
                    -- messages may have been the ones that got interrupted
                    void syncReplay
                    -- now wait for the person doing ghciExec to have actually
                    -- left the lock
                    Concurrent.Extra.withLock isRunning pass
                    -- there may have been two syncs sent, so now do a fresh sync
                    -- to clear everything
                    stop <- syncFresh
                    void $ consume2 "Interrupt" \_ s -> pure $ if stop s then Just () else Nothing

        ghciUnique <- Unique.newUnique
        let ghci = Ghci{..}

        -- Now wait for 'GHCi, version' to appear before sending anything real,
        -- required for #57
        stdout <- newIORef []
        stderr <- newIORef []
        sync <- newIORef $ const False
        void $ consume2 "" \strm s0 -> do
            stop <- readIORef sync
            if stop s0 then
                pure $ Just ()
            else do
                -- there may be some initial prompts on stdout before I set the
                -- prompt properly
                let s = maybe s0 (removePrefix . snd) $ List.Extra.stripInfix ghcid_prefix s0
                Verbosity.whenLoud $ Util.outStrLn $ "%STDOUT2: " <> s
                modifyIORef (if strm == Stdout then stdout else stderr) (s:)
                when (any (`isPrefixOf` s) [ "GHCi, version "
                                           , "GHCJSi, version "
                                           , "Clashi, version " ]) do
                    -- the thing before me may have done its own Haskell
                    -- compiling
                    writeIORef stdout []
                    writeIORef stderr []
                    writeInp "import qualified System.IO as INTERNAL_GHCID"
                    writeInp ":unset +t +s" -- see https://github.com/ndmitchell/ghcid/issues/162
                    writeInp $ ":set prompt " <> ghcid_prefix
                    writeInp $ ":set prompt-cont " <> ghcid_prefix

                    -- failure isn't harmful, so do them one-by-one
                    forM_ (Util.ghciFlagsRequired <> Util.ghciFlagsRequiredVersioned) \flag ->
                        writeInp $ ":set " <> flag
                    writeIORef sync =<< syncFresh
                echo0 strm s
                pure Nothing
        r1 <- Parser.parseLoad . reverse <$> ((<>) <$> readIORef stderr <*> readIORef stdout)
        -- see #132, if hide-source-paths was turned on the modules didn't get
        -- printed out properly so try a showModules to capture the information
        -- again
        r2 <- if any isLoading r1 then pure [] else map (uncurry Loading) <$> showModules ghci
        execStream ghci "" echo0
        pure (ghci, r1 <> r2)


-- | Start GHCi by running the given shell command, a helper around
--   'startGhciProcess'.
startGhci
    :: String -- ^ Shell command
    -> Maybe FilePath -- ^ Working directory
    -> (Stream -> String -> IO ()) -- ^ Output callback
    -> IO (Ghci, [Load])
startGhci cmd directory =
  startGhciProcess (Process.shell cmd){ Process.cwd = directory }


-- | Execute a command, calling a callback on each response.
--   The callback will be called single threaded.
execStream :: Ghci -> String -> (Stream -> String -> IO ()) -> IO ()
execStream = ghciExec

-- | Interrupt Ghci, stopping the current computation (if any),
--   but leaving the process open to new input.
interrupt :: Ghci -> IO ()
interrupt = ghciInterrupt

-- | Obtain the progress handle behind a GHCi instance.
process :: Ghci -> Process.ProcessHandle
process = ghciProcess


---------------------------------------------------------------------------------
-- SUGAR HELPERS

-- | Execute a command, calling a callback on each response. The callback will
--   be called single threaded.
execBuffer :: Ghci -> String -> (Stream -> String -> IO ()) -> IO [String]
execBuffer ghci cmd echo = do
  stdout <- newIORef []
  stderr <- newIORef []
  execStream ghci cmd \strm s -> do
    modifyIORef (if strm == Stdout then stdout else stderr) (s:)
    echo strm s
  reverse <$> ((<>) <$> readIORef stderr <*> readIORef stdout)

-- | Send a command, get lines of result. Must be called single-threaded.
exec :: Ghci -> String -> IO [String]
exec ghci cmd = execBuffer ghci cmd \_ _ -> pass

-- | List the modules currently loaded, with module name and source file.
showModules :: Ghci -> IO [(String,FilePath)]
showModules ghci = Parser.parseShowModules <$> exec ghci ":show modules"

-- | Return the current working directory, and a list of module import paths
showPaths :: Ghci -> IO (FilePath, [FilePath])
showPaths ghci = Parser.parseShowPaths <$> exec ghci ":show paths"

-- | Perform a reload, list the messages that reload generated.
reload :: Ghci -> IO [Load]
reload ghci = Parser.parseLoad <$> exec ghci ":reload"

-- | Send @:quit@ and wait for the process to quit.
quit :: Ghci -> IO ()
quit ghci =  do
  interrupt ghci
  Exception.handle (\UnexpectedExit{} -> pass) $ void $ exec ghci ":quit"
  -- Be aware that waitForProcess has a race condition, see
  -- https://github.com/haskell/process/issues/46. Therefore just ignore the
  -- exception anyway, its probably already terminated.
  Util.ignored $ void $ Process.waitForProcess $ process ghci
