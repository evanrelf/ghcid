{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- | The application entry point
module Main (main) where

import CliOptions (CliOptions (..), ColorMode (..))
import Data.Version (Version)
import Ghcid.Escape (Esc (..))
import Ghcid.Types
  ( EvalResult (..)
  , GhciError (..)
  , Load (..)
  , Severity (..)
  , isMessage
  )
import Relude.Extra.Enum (prev)
import Relude.Extra.Tuple (traverseToSnd)
import Session (Session)
import System.Console.CmdArgs (Verbosity (..))
import System.FilePath ((</>))

import qualified CliOptions
import qualified Control.Exception as Exception
import qualified Control.Monad.Extra as Monad.Extra
import qualified Data.List as List
import qualified Data.List.Extra as List.Extra
import qualified Data.String as String
import qualified Data.Tuple.Extra as Tuple.Extra
import qualified Data.Version as Version
import qualified Ghcid.Escape as Escape
import qualified Ghcid.Util as Util
import qualified Session
import qualified System.Console.ANSI as Ansi
import qualified System.Console.CmdArgs.Verbosity as Verbosity
import qualified System.Console.Terminal.Size as Term
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified System.IO as IO
import qualified System.IO.Error as IO.Error
import qualified System.Info
import qualified System.Process as Process
import qualified Wait

version :: Version
version = Version.makeVersion [0,0]

{-
What happens on various command lines:

Hlint with no .ghci file:
- cabal repl - prompt with Language.Haskell.HLint loaded
- cabal exec ghci Sample.hs - prompt with Sample.hs loaded
- ghci - prompt with nothing loaded
- ghci Sample.hs - prompt with Sample.hs loaded
- stack ghci - prompt with all libraries and Main loaded

Hlint with a .ghci file:
- cabal repl - loads everything twice, prompt with Language.Haskell.HLint loaded
- cabal exec ghci Sample.hs - loads everything first, then prompt with Sample.hs loaded
- ghci - prompt with everything
- ghci Sample.hs - loads everything first, then prompt with Sample.hs loaded
- stack ghci - loads everything first, then prompt with libraries and Main loaded

Warnings:
- cabal repl won't pull in any C files (e.g. hoogle)
- cabal exec ghci won't work with modules that import an autogen Paths module

As a result, we prefer to give users full control with a .ghci file, if available
-}
autoOptions :: CliOptions -> IO CliOptions
autoOptions o@CliOptions{..}
  | command /= "" = pure $ f [command] []
  | otherwise = do
      curdir <- Directory.getCurrentDirectory
      files <- Directory.getDirectoryContents "."

      -- use unsafePerformIO to get nicer pattern matching for logic (read-only operations)
      let findStack dir = flip IO.Error.catchIOError (const $ pure Nothing) do
              let yaml = dir </> "stack.yaml"
              b <- Directory.doesFileExist yaml &&^ Directory.doesDirectoryExist (dir </> ".stack-work")
              pure $ if b then Just yaml else Nothing
      stackFile <- Monad.Extra.firstJustM findStack [".",".."] -- stack file might be parent, see #62

      let cabal = map (curdir </>) $ filter ((==) ".cabal" . FilePath.takeExtension) files
      let isLib = isPrefixOf "lib:"  -- `lib:foo` is the Cabal format
      let noCode =
            [ "-fno-code"
            |  null test
            && not allow_eval
            && (isJust stackFile || all isLib target)
            ]
      let opts = noCode <> Util.ghciFlagsRequired <> Util.ghciFlagsUseful
      pure $ case () of
          _ | Just stack <- stackFile ->
              let flags = if null arguments then
                                 ("stack ghci" : target)
                              <> ("--test --bench" : ["--no-load" | ".ghci" `elem` files])
                              <> map ("--ghci-options=" <>) opts
                          else
                              "stack exec --test --bench -- ghci" : opts
              in f flags $ stack:cabal
            | ".ghci" `elem` files -> f ("ghci":opts) [curdir </> ".ghci"]
            | cabal /= [] ->
                let useCabal = ["cabal","repl"] <> target <> map ("--repl-options=" <>) opts
                    useGhci = "cabal exec -- ghci":opts
                in  f (if null arguments then useCabal else useGhci) cabal
            | otherwise -> f ("ghci":opts) []
  where
    f c r = o{command = String.unwords $ c <> map escape arguments, arguments = [], restart = restart <> r, test = [] <> test}

-- | Simple escaping for command line arguments. Wraps a string in double quotes if it contains a space.
escape :: String -> String
escape x
  | ' ' `elem` x = "\"" <> x <> "\""
  | otherwise = x

-- | Use arguments from .ghcid if present
withGhcidArgs :: IO a -> IO a
withGhcidArgs action = do
  exists <- Directory.doesFileExist ".ghcid"

  if exists then do
    fileContents <- readFileText ".ghcid"
    let extraArguments = toString <$> concatMap words (lines fileContents)
    originalArguments <- fmap toString <$> Environment.getArgs
    Environment.withArgs (extraArguments <> originalArguments) action

  else
    action
  
data TermSize = TermSize
  { termWidth :: Int
  , termHeight :: Maybe Int -- ^ Nothing means the height is unlimited
  , termWrap :: Escape.WordWrap
  }

-- | On the 'UnexpectedExit' exception exit with a nice error message.
handleErrors :: IO () -> IO ()
handleErrors = Exception.handle \(UnexpectedExit cmd _ mmsg) -> do
  putStr $ "Command \"" <> cmd <> "\" exited unexpectedly"
  putStrLn $ case mmsg of
    Just msg -> " with error message: " <> msg
    Nothing -> ""
  exitFailure

printStopped :: CliOptions -> IO ()
printStopped opts =
  forM_ (outputfile opts) \file -> writeFile file "Ghcid has stopped.\n"


-- | Like 'main', but run with a fake terminal for testing
mainWithTerminal :: IO TermSize -> ([String] -> IO ()) -> IO ()
mainWithTerminal termSize termOutput = do
  opts <- withGhcidArgs CliOptions.getCliOptions
  Verbosity.whenLoud do
    Util.outStrLn $ "%OS: " <> System.Info.os
    Util.outStrLn $ "%ARCH: " <> System.Info.arch
    Util.outStrLn $ "%VERSION: " <> Version.showVersion version
    args <- Environment.getArgs
    Util.outStrLn $ "%ARGUMENTS: " <> show args
  flip Exception.finally (printStopped opts) $ handleErrors $
    forever $ Session.withSession \session -> do
      Verbosity.setVerbosity Normal -- undo any --verbose flags

      -- On certain Cygwin terminals stdout defaults to BlockBuffering
      IO.hSetBuffering stdout IO.LineBuffering
      IO.hSetBuffering stderr IO.NoBuffering
      origDir <- Directory.getCurrentDirectory
      Directory.withCurrentDirectory (directory opts) do
        opts <- autoOptions opts
        opts <- pure $ opts{restart = List.Extra.nubOrd $ (origDir </> ".ghcid") : restart opts, reload = List.Extra.nubOrd $ reload opts}

        let noHeight = if no_height_limit opts then const Nothing else id
        termSize <- pure $ case (width opts, height opts) of
          (Just w, Just h) -> pure $ TermSize w (noHeight $ Just h) Escape.WrapHard
          (w, h) -> do
            term <- termSize
            -- if we write to the final column of the window then it wraps automatically
            -- so putStrLn width 'x' uses up two lines
            pure $ TermSize
              (fromMaybe (prev $ termWidth term) w)
              (noHeight $ h <|> termHeight term)
              (if isJust w then Escape.WrapHard else termWrap term)

        restyle <- do
          useStyle <- case color opts of
            Always -> pure True
            Never -> pure False
            Auto -> Ansi.hSupportsANSI stdout
          when useStyle do
            h <- Environment.lookupEnv "HSPEC_OPTIONS"
            when (isNothing h) $ Environment.setEnv "HSPEC_OPTIONS" "--color" -- see #87
          pure $ if useStyle then id else map Escape.unescape

        clear <- pure $
          if clear opts
          then (Ansi.clearScreen *>)
          else id

        maybe Wait.withWaiterNotify Wait.withWaiterPoll (poll opts) \waiter ->
          runGhcid (if allow_eval opts then Session.enableEval session else session) waiter termSize (clear . termOutput . restyle) opts


main :: IO ()
main = mainWithTerminal termSize termOutput
  where
    termSize =
      Term.size <&> \case
        Nothing -> TermSize 80 (Just 8) Escape.WrapHard
        Just t -> TermSize (Term.width t) (Just $ Term.height t) Escape.WrapSoft

    termOutput xs = do
      Util.outStr $ concatMap ('\n':) xs
      IO.hFlush stdout -- must flush, since we don't finish with a newline


data Continue = Continue

data ReloadMode
  = Reload
  | Restart
  deriving stock (Show, Ord, Eq)

-- If we pure successfully, we restart the whole process
-- Use Continue not () so that inadvertant exits don't restart
runGhcid :: Session -> Wait.Waiter -> IO TermSize -> ([String] -> IO ()) -> CliOptions -> IO Continue
runGhcid session waiter termSize termOutput CliOptions{..} = do
  let limitMessages = maybe id (take . max 1) max_messages

  let outputFill :: String -> Maybe (Int, [Load]) -> [EvalResult] -> [String] -> IO ()
      outputFill currTime load0 evals msg = do
        let load = case load0 of
              Nothing -> []
              Just (loadedCount, msgs) -> prettyOutput currTime loadedCount (filter isMessage msgs) evals
        TermSize{..} <- termSize
        let wrap = concatMap (Escape.wordWrapE termWidth (termWidth `div` 5) . Esc)
        (msg, load, pad) <-
            case termHeight of
                Nothing -> pure (wrap msg, wrap load, [])
                Just termHeight -> do
                    (termHeight, msg) <- pure $ Util.takeRemainder termHeight $ wrap msg
                    (termHeight, load) <-
                        let takeRemainder' =
                                if reverse_errors
                                then -- When reversing the errors we want to crop out
                                     -- the top instead of the bottom of the load
                                     fmap reverse . Util.takeRemainder termHeight . reverse
                                else Util.takeRemainder termHeight
                        in pure $ takeRemainder' $ wrap load
                    pure (msg, load, replicate termHeight "")
        let mergeSoft ((Esc x,Escape.WrapSoft):(Esc y,q):xs) = mergeSoft $ (Esc (x<>y), q) : xs
            mergeSoft ((x,_):xs) = x : mergeSoft xs
            mergeSoft [] = []

            applyPadding x =
                if reverse_errors
                then pad <> x
                else x <> pad
        termOutput $ applyPadding $ map fromEsc ((if termWrap == Escape.WrapSoft then mergeSoft else map fst) $ load <> msg)

  when (ignoreLoaded && null reload) do
    putStrLn "--reload must be set when using --ignore-loaded"
    exitFailure

  nextWait0 <- Wait.waitFiles waiter
  (messages, loaded) <- Session.sessionStart session command $
    map (":set " <>) (Util.ghciFlagsUseful <> Util.ghciFlagsUsefulVersioned) <> setup

  when (null loaded && not ignoreLoaded) do
    putStrLn $ "\nNo files loaded, GHCi is not working properly.\nCommand: " <> command
    exitFailure

  let restart = List.Extra.nubOrd $ restart <> [x | LoadConfig x <- messages]

  project <- if project /= "" then pure project else FilePath.takeFileName <$> Directory.getCurrentDirectory

  -- fire, given a waiter, the messages/loaded/touched
  let
    fire
      :: ([(FilePath, ReloadMode)] -> IO (Either String [(FilePath, ReloadMode)]))
      -> ([Load], [FilePath], [FilePath])
      -> IO Continue
    fire nextWait (messages, loaded, touched) = do
          currTime0 <- Util.getShortTime
          let loadedCount = length loaded
          Verbosity.whenLoud do
              Util.outStrLn $ "%MESSAGES: " <> show messages
              Util.outStrLn $ "%LOADED: " <> show loaded

          let evals = [e | Eval e <- messages]
          let (countErrors, countWarnings) = Tuple.Extra.both (sum :: [Int] -> Int) $ unzip
                  [if loadSeverity == Error then (1,0) else (0,1) | Message{..} <- messages, loadMessage /= []]
          let hasErrors = countErrors /= 0 || (countWarnings /= 0 && not warnings)
          let test1 =
                  if null test || hasErrors then Nothing
                  else Just $ intercalate "\n" test

          let updateTitle extra = unless no_title $ Ansi.setTitle $ Escape.unescape $
                  let f n msg = if n == 0 then "" else show n <> " " <> msg <> ['s' | n > 1]
                  in (if countErrors == 0 && countWarnings == 0 then Util.allGoodMessage <> ", at " <> currTime0 else f countErrors "error" <>
                     (if countErrors >  0 && countWarnings >  0 then ", " else "") <> f countWarnings "warning") <>
                     " " <> extra <> [' ' | extra /= ""] <> "- " <> project

          updateTitle $ if isJust test1 then "(running test)" else ""

          -- order and restrict the messages
          -- nubOrdOn loadMessage because module cycles generate the same message at several different locations
          ordMessages <- do
              let (msgError, msgWarn) = List.partition ((==) Error . loadSeverity) $ List.Extra.nubOrdOn loadMessage $ filter isMessage messages
              -- sort error messages by modtime, so newer edits cause the errors to float to the top - see #153
              errTimes <- sequence [traverseToSnd Util.getModTime x | x <- List.Extra.nubOrd $ map loadFile msgError]
              let f x = List.lookup (loadFile x) errTimes
                  moduleSorted = sortOn (Down . f) msgError <> msgWarn
              pure $ (if reverse_errors then reverse else id) moduleSorted

          outputFill currTime0 (Just (loadedCount, ordMessages)) evals [test_message | isJust test1]
          forM_ outputfile \file ->
              writeFile file $
                  if FilePath.takeExtension file == ".json" then
                      showJSON [("loaded",map jString loaded),("messages",map jMessage $ filter isMessage messages)]
                  else
                      String.unlines $ map Escape.unescape $ prettyOutput currTime0 loadedCount (limitMessages ordMessages) evals
          when (null loaded && not ignoreLoaded) do
              putStrLn "No files loaded, nothing to wait for. Fix the last error and restart."
              exitFailure
          whenJust test1 \t -> do
              Verbosity.whenLoud $ Util.outStrLn $ "%TESTING: " <> t
              Session.sessionExecAsync session t \stderr -> do
                  Verbosity.whenLoud $ Util.outStrLn "%TESTING: Completed"
                  IO.hFlush stdout -- may not have been a terminating newline from test output
                  if "*** Exception: " `isPrefixOf` stderr then do
                      updateTitle "(test failed)"
                   else do
                      updateTitle "(test done)"
                      Verbosity.whenNormal $ Util.outStrLn "\n...done"
          whenJust lint \lintcmd ->
              unless hasErrors do
                  (exitcode, stdout, stderr) <- Process.readCreateProcessWithExitCode (Process.shell . String.unwords $ lintcmd : map escape touched) ""
                  unless (exitcode == Exit.ExitSuccess) $ Util.outStrLn (stdout <> stderr)

          reason <- nextWait $ map (,Restart) restart
                            <> map (,Reload) reload
                            <> map (,Reload) loaded

          let reason1 = case reason of
                Left err ->
                  (Reload, ["Error when waiting, if this happens repeatedly, raise a ghcid bug.", err])
                Right files ->
                  case List.partition (\(_f, mode') -> mode' == Reload) files of
                    -- Prefer restarts over reloads. E.g., in case of both '--reload=dir'
                    -- and '--restart=dir', ghcid would restart instead of reload.
                    (_, rs@(_:_)) -> (Restart, map fst rs)
                    (rl, _) -> (Reload, map fst rl)

          currTime <- Util.getShortTime
          case reason1 of
            (Reload, reason2) -> do
              unless no_status $ outputFill currTime Nothing evals $ "Reloading..." : map ("  " <>) reason2
              nextWait1 <- Wait.waitFiles waiter
              fire nextWait1 =<< Session.sessionReload session
            (Restart, reason2) -> do
              -- exit cleanly, since the whole thing is wrapped in a forever
              unless no_status $ outputFill currTime Nothing evals $ "Restarting..." : map ("  " <>) reason2
              pure Continue

  fire nextWait0 (messages, loaded, loaded)


-- | Given an available height, and a set of messages to display, show them as best you can.
prettyOutput :: String -> Int -> [Load] -> [EvalResult] -> [String]
prettyOutput currTime loadedCount [] evals =
  (Util.allGoodMessage <> " (" <> show loadedCount <> " module" <> ['s' | loadedCount /= 1] <> ", at " <> currTime <> ")")
    : concatMap printEval evals
prettyOutput _ _ xs evals = concatMap loadMessage xs <> concatMap printEval evals

printEval :: EvalResult -> [String]
printEval (EvalResult file (line, col) msg result) =
  [ " "
  , concat
      [ file
      , ":"
      , show line
      , ":"
      , show col
      ]
  ] <> map ("$> " <>) (String.lines msg)
    <> String.lines result


showJSON :: [(String, [String])] -> String
showJSON xs = String.unlines $ concat $
  [ ((if i == 0 then "{" else ",") <> jString a <> ":") :
    ["  " <> (if j == 0 then "[" else ",") <> b | (j,b) <- zip [0 :: Int, 1 ..] bs] <>
    [if null bs then "  []" else "  ]"]
  | (i,(a,bs)) <- zip [0 :: Int, 1 ..] xs] <>
  [["}"]]

jString :: String -> String
jString x = "\"" <> List.Extra.escapeJSON x <> "\""

jMessage :: Load -> String
jMessage Message{..} = jDict $
  [("severity",jString $ show loadSeverity)
  ,("file",jString loadFile)] <>
  [("start",pair loadFilePos) | loadFilePos /= (0,0)] <>
  [("end", pair loadFilePosEnd) | loadFilePos /= loadFilePosEnd] <>
  [("message", jString $ intercalate "\n" loadMessage)]
  where pair (a,b) = "[" <> show a <> "," <> show b <> "]"
jMessage _ = error (toText "jMessage: Incomplete patterns")

jDict :: [(String, String)] -> String
jDict xs = "{" <> intercalate ", " [jString a <> ":" <> b | (a,b) <- xs] <> "}"
