{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- | The application entry point
module Main (main) where

import Control.Exception
import Data.Tuple.Extra
import Data.Version
import Relude.Extra.Enum (prev)
import Relude.Extra.Tuple (traverseToSnd)
import Session
import System.Console.ANSI
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit
import System.Environment
import System.FilePath ((</>))
import System.IO.Error
import System.Info
import System.Process
import System.Time.Extra

import qualified Control.Monad.Extra as Monad
import qualified Data.List.Extra as List
import qualified Data.String as String
import qualified System.Console.Terminal.Size as Term
import qualified System.Directory.Extra as Directory
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified System.IO.Extra as IO

import Ghcid.Escape
import Ghcid.Types
import Ghcid.Util
import Wait

-- | Command line options
data Options = Options
  { command :: String
  , arguments :: [String]
  , test :: [String]
  , test_message :: String
  , run :: [String]
  , warnings :: Bool
  , lint :: Maybe String
  , no_status :: Bool
  , clear :: Bool
  , reverse_errors :: Bool
  , no_height_limit :: Bool
  , height :: Maybe Int
  , width :: Maybe Int
  , no_title :: Bool
  , project :: String
  , reload :: [FilePath]
  , restart :: [FilePath]
  , directory :: FilePath
  , outputfile :: [FilePath]
  , ignoreLoaded :: Bool
  , poll :: Maybe Seconds
  , max_messages :: Maybe Int
  , color :: ColorMode
  , setup :: [String]
  , allow_eval :: Bool
  , target :: [String]
  } deriving stock (Data, Typeable, Show)

-- | When to colour terminal output.
data ColorMode
  = Never  -- ^ Terminal output will never be coloured.
  | Always -- ^ Terminal output will always be coloured.
  | Auto   -- ^ Terminal output will be coloured if $TERM and stdout appear to support it.
  deriving stock (Show, Typeable, Data)

version :: Version
version = makeVersion [0,0]

options :: Mode (CmdArgs Options)
options = cmdArgsMode $ Options
  { command = ""
      &= name "c"
      &= typ "COMMAND"
      &= help "Command to run (defaults to ghci or cabal repl)"
  , arguments = []
      &= args
      &= typ "MODULE"
  , test = []
      &= name "T"
      &= typ "EXPR"
      &= help "Command to run after successful loading"
  , test_message = "Running test..."
      &= typ "MESSAGE"
      &= help "Message to show before running the test (defaults to \"Running test...\")"
  , run = []
      &= name "r"
      &= typ "EXPR"
      &= opt "main"
      &= help "Command to run after successful loading (like --test but defaults to main)"
  , warnings = False
      &= name "W"
      &= help "Allow tests to run even with warnings"
  , lint = Nothing
      &= typ "COMMAND"
      &= name "lint"
      &= opt "hlint"
      &= help "Linter to run if there are no errors. Defaults to hlint."
  , no_status = False
      &= name "S"
      &= help "Suppress status messages"
  , clear = False
      &= name "clear"
      &= help "Clear screen when reloading"
  , reverse_errors = False
      &= help "Reverse output order (works best with --no-height-limit)"
  , no_height_limit = False
      &= name "no-height-limit"
      &= help "Disable height limit"
  , height = Nothing
      &= help "Number of lines to use (defaults to console height)"
  , width = Nothing
      &= name "w"
      &= help "Number of columns to use (defaults to console width)"
  , no_title = False
      &= help "Don't update the shell title/icon"
  , project = ""
      &= typ "NAME"
      &= help "Name of the project, defaults to current directory"
  , restart = []
      &= typ "PATH"
      &= help "Restart the command when the given file or directory contents change (defaults to .ghci and any .cabal file, unless when using stack or a custom command)"
  , reload = []
      &= typ "PATH"
      &= help "Reload when the given file or directory contents change (defaults to none)"
  , directory = "."
      &= typDir
      &= name "C"
      &= help "Set the current directory"
  , outputfile = []
      &= typFile
      &= name "o"
      &= help "File to write the full output to"
  , ignoreLoaded = False
      &= explicit
      &= name "ignore-loaded"
      &= help "Keep going if no files are loaded. Requires --reload to be set."
  , poll = Nothing
      &= typ "SECONDS"
      &= opt "0.1"
      &= explicit
      &= name "poll"
      &= help "Use polling every N seconds (defaults to using notifiers)"
  , max_messages = Nothing
      &= name "n"
      &= help "Maximum number of messages to print"
  , color = Auto
      &= name "colour"
      &= name "color"
      &= opt Always
      &= typ "always/never/auto"
      &= help "Color output (defaults to when the terminal supports it)"
  , setup = []
      &= name "setup"
      &= typ "COMMAND"
      &= help "Setup commands to pass to ghci on stdin, usually :set <something>"
  , allow_eval = False
      &= name "allow-eval"
      &= help "Execute REPL commands in comments"
  , target = []
      &= typ "TARGET"
      &= help "Target Component to build (e.g. lib:foo for Cabal, foo:lib for Stack)"
  }
      &= verbosity
      &=
  program "ghcid"
        &= summary ("Auto reloading GHCi daemon v" <> showVersion version)


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
autoOptions :: Options -> IO Options
autoOptions o@Options{..}
  | command /= "" = pure $ f [command] []
  | otherwise = do
      curdir <- Directory.getCurrentDirectory
      files <- Directory.getDirectoryContents "."

      -- use unsafePerformIO to get nicer pattern matching for logic (read-only operations)
      let findStack dir = flip catchIOError (const $ pure Nothing) do
              let yaml = dir </> "stack.yaml"
              b <- Directory.doesFileExist yaml &&^ Directory.doesDirectoryExist (dir </> ".stack-work")
              pure $ if b then Just yaml else Nothing
      stackFile <- Monad.firstJustM findStack [".",".."] -- stack file might be parent, see #62

      let cabal = map (curdir </>) $ filter ((==) ".cabal" . FilePath.takeExtension) files
      let isLib = isPrefixOf "lib:"  -- `lib:foo` is the Cabal format
      let noCode =
            [ "-fno-code"
            |  null test
            && null run
            && not allow_eval
            && (isJust stackFile || all isLib target)
            ]
      let opts = noCode <> ghciFlagsRequired <> ghciFlagsUseful
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
    f c r = o{command = String.unwords $ c <> map escape arguments, arguments = [], restart = restart <> r, run = [], test = run <> test}

-- | Simple escaping for command line arguments. Wraps a string in double quotes if it contains a space.
escape :: String -> String
escape x
  | ' ' `elem` x = "\"" <> x <> "\""
  | otherwise = x

-- | Use arguments from .ghcid if present
withGhcidArgs :: IO a -> IO a
withGhcidArgs act = do
  b <- Directory.doesFileExist ".ghcid"
  if not b then act else do
    extra <- concatMap splitArgs . String.lines <$> IO.readFile' ".ghcid"
    orig <- getArgs
    withArgs (extra <> orig) act


data TermSize = TermSize
  { termWidth :: Int
  , termHeight :: Maybe Int -- ^ Nothing means the height is unlimited
  , termWrap :: WordWrap
  }

-- | On the 'UnexpectedExit' exception exit with a nice error message.
handleErrors :: IO () -> IO ()
handleErrors = handle \(UnexpectedExit cmd _ mmsg) -> do
  putStr $ "Command \"" <> cmd <> "\" exited unexpectedly"
  putStrLn $ case mmsg of
    Just msg -> " with error message: " <> msg
    Nothing -> ""
  exitFailure

printStopped :: Options -> IO ()
printStopped opts =
  forM_ (outputfile opts) \file -> writeFile file "Ghcid has stopped.\n"


-- | Like 'main', but run with a fake terminal for testing
mainWithTerminal :: IO TermSize -> ([String] -> IO ()) -> IO ()
mainWithTerminal termSize termOutput = do
  opts <- withGhcidArgs $ cmdArgsRun options
  whenLoud do
    outStrLn $ "%OS: " <> os
    outStrLn $ "%ARCH: " <> arch
    outStrLn $ "%VERSION: " <> showVersion version
    args <- getArgs
    outStrLn $ "%ARGUMENTS: " <> show args
  flip finally (printStopped opts) $ handleErrors $
    forever $ withSession \session -> do
      setVerbosity Normal -- undo any --verbose flags

      -- On certain Cygwin terminals stdout defaults to BlockBuffering
      IO.hSetBuffering stdout IO.LineBuffering
      IO.hSetBuffering stderr IO.NoBuffering
      origDir <- Directory.getCurrentDirectory
      Directory.withCurrentDirectory (directory opts) do
        opts <- autoOptions opts
        opts <- pure $ opts{restart = List.nubOrd $ (origDir </> ".ghcid") : restart opts, reload = List.nubOrd $ reload opts}

        let noHeight = if no_height_limit opts then const Nothing else id
        termSize <- pure $ case (width opts, height opts) of
          (Just w, Just h) -> pure $ TermSize w (noHeight $ Just h) WrapHard
          (w, h) -> do
            term <- termSize
            -- if we write to the final column of the window then it wraps automatically
            -- so putStrLn width 'x' uses up two lines
            pure $ TermSize
              (fromMaybe (prev $ termWidth term) w)
              (noHeight $ h <|> termHeight term)
              (if isJust w then WrapHard else termWrap term)

        restyle <- do
          useStyle <- case color opts of
            Always -> pure True
            Never -> pure False
            Auto -> hSupportsANSI stdout
          when useStyle do
            h <- lookupEnv "HSPEC_OPTIONS"
            when (isNothing h) $ setEnv "HSPEC_OPTIONS" "--color" -- see #87
          pure $ if useStyle then id else map unescape

        clear <- pure $
          if clear opts
          then (clearScreen *>)
          else id

        maybe withWaiterNotify withWaiterPoll (poll opts) \waiter ->
          runGhcid (if allow_eval opts then enableEval session else session) waiter termSize (clear . termOutput . restyle) opts



main :: IO ()
main = mainWithTerminal termSize termOutput
  where
    termSize =
      Term.size <&> \case
        Nothing -> TermSize 80 (Just 8) WrapHard
        Just t -> TermSize (Term.width t) (Just $ Term.height t) WrapSoft

    termOutput xs = do
      outStr $ concatMap ('\n':) xs
      IO.hFlush stdout -- must flush, since we don't finish with a newline


data Continue = Continue

data ReloadMode
  = Reload
  | Restart
  deriving stock (Show, Ord, Eq)

-- If we pure successfully, we restart the whole process
-- Use Continue not () so that inadvertant exits don't restart
runGhcid :: Session -> Waiter -> IO TermSize -> ([String] -> IO ()) -> Options -> IO Continue
runGhcid session waiter termSize termOutput Options{..} = do
  let limitMessages = maybe id (take . max 1) max_messages

  let outputFill :: String -> Maybe (Int, [Load]) -> [EvalResult] -> [String] -> IO ()
      outputFill currTime load0 evals msg = do
        let load = case load0 of
              Nothing -> []
              Just (loadedCount, msgs) -> prettyOutput currTime loadedCount (filter isMessage msgs) evals
        TermSize{..} <- termSize
        let wrap = concatMap (wordWrapE termWidth (termWidth `div` 5) . Esc)
        (msg, load, pad) <-
            case termHeight of
                Nothing -> pure (wrap msg, wrap load, [])
                Just termHeight -> do
                    (termHeight, msg) <- pure $ takeRemainder termHeight $ wrap msg
                    (termHeight, load) <-
                        let takeRemainder' =
                                if reverse_errors
                                then -- When reversing the errors we want to crop out
                                     -- the top instead of the bottom of the load
                                     fmap reverse . takeRemainder termHeight . reverse
                                else takeRemainder termHeight
                        in pure $ takeRemainder' $ wrap load
                    pure (msg, load, replicate termHeight "")
        let mergeSoft ((Esc x,WrapSoft):(Esc y,q):xs) = mergeSoft $ (Esc (x<>y), q) : xs
            mergeSoft ((x,_):xs) = x : mergeSoft xs
            mergeSoft [] = []

            applyPadding x =
                if reverse_errors
                then pad <> x
                else x <> pad
        termOutput $ applyPadding $ map fromEsc ((if termWrap == WrapSoft then mergeSoft else map fst) $ load <> msg)

  when (ignoreLoaded && null reload) do
    putStrLn "--reload must be set when using --ignore-loaded"
    exitFailure

  nextWait0 <- waitFiles waiter
  (messages, loaded) <- sessionStart session command $
    map (":set " <>) (ghciFlagsUseful <> ghciFlagsUsefulVersioned) <> setup

  when (null loaded && not ignoreLoaded) do
    putStrLn $ "\nNo files loaded, GHCi is not working properly.\nCommand: " <> command
    exitFailure

  let restart = List.nubOrd $ restart <> [x | LoadConfig x <- messages]

  project <- if project /= "" then pure project else FilePath.takeFileName <$> Directory.getCurrentDirectory

  -- fire, given a waiter, the messages/loaded/touched
  let
    fire
      :: ([(FilePath, ReloadMode)] -> IO (Either String [(FilePath, ReloadMode)]))
      -> ([Load], [FilePath], [FilePath])
      -> IO Continue
    fire nextWait (messages, loaded, touched) = do
          currTime0 <- getShortTime
          let loadedCount = length loaded
          whenLoud do
              outStrLn $ "%MESSAGES: " <> show messages
              outStrLn $ "%LOADED: " <> show loaded

          let evals = [e | Eval e <- messages]
          let (countErrors, countWarnings) = both (sum :: [Int] -> Int) $ unzip
                  [if loadSeverity == Error then (1,0) else (0,1) | Message{..} <- messages, loadMessage /= []]
          let hasErrors = countErrors /= 0 || (countWarnings /= 0 && not warnings)
          let test1 =
                  if null test || hasErrors then Nothing
                  else Just $ intercalate "\n" test

          let updateTitle extra = unless no_title $ setTitle $ unescape $
                  let f n msg = if n == 0 then "" else show n <> " " <> msg <> ['s' | n > 1]
                  in (if countErrors == 0 && countWarnings == 0 then allGoodMessage <> ", at " <> currTime0 else f countErrors "error" <>
                     (if countErrors >  0 && countWarnings >  0 then ", " else "") <> f countWarnings "warning") <>
                     " " <> extra <> [' ' | extra /= ""] <> "- " <> project

          updateTitle $ if isJust test1 then "(running test)" else ""

          -- order and restrict the messages
          -- nubOrdOn loadMessage because module cycles generate the same message at several different locations
          ordMessages <- do
              let (msgError, msgWarn) = List.partition ((==) Error . loadSeverity) $ List.nubOrdOn loadMessage $ filter isMessage messages
              -- sort error messages by modtime, so newer edits cause the errors to float to the top - see #153
              errTimes <- sequence [traverseToSnd getModTime x | x <- List.nubOrd $ map loadFile msgError]
              let f x = List.lookup (loadFile x) errTimes
                  moduleSorted = sortOn (Down . f) msgError <> msgWarn
              pure $ (if reverse_errors then reverse else id) moduleSorted

          outputFill currTime0 (Just (loadedCount, ordMessages)) evals [test_message | isJust test1]
          forM_ outputfile \file ->
              writeFile file $
                  if FilePath.takeExtension file == ".json" then
                      showJSON [("loaded",map jString loaded),("messages",map jMessage $ filter isMessage messages)]
                  else
                      String.unlines $ map unescape $ prettyOutput currTime0 loadedCount (limitMessages ordMessages) evals
          when (null loaded && not ignoreLoaded) do
              putStrLn "No files loaded, nothing to wait for. Fix the last error and restart."
              exitFailure
          whenJust test1 \t -> do
              whenLoud $ outStrLn $ "%TESTING: " <> t
              sessionExecAsync session t \stderr -> do
                  whenLoud $ outStrLn "%TESTING: Completed"
                  IO.hFlush stdout -- may not have been a terminating newline from test output
                  if "*** Exception: " `isPrefixOf` stderr then do
                      updateTitle "(test failed)"
                   else do
                      updateTitle "(test done)"
                      whenNormal $ outStrLn "\n...done"
          whenJust lint \lintcmd ->
              unless hasErrors do
                  (exitcode, stdout, stderr) <- readCreateProcessWithExitCode (shell . String.unwords $ lintcmd : map escape touched) ""
                  unless (exitcode == Exit.ExitSuccess) $ outStrLn (stdout <> stderr)

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

          currTime <- getShortTime
          case reason1 of
            (Reload, reason2) -> do
              unless no_status $ outputFill currTime Nothing evals $ "Reloading..." : map ("  " <>) reason2
              nextWait1 <- waitFiles waiter
              fire nextWait1 =<< sessionReload session
            (Restart, reason2) -> do
              -- exit cleanly, since the whole thing is wrapped in a forever
              unless no_status $ outputFill currTime Nothing evals $ "Restarting..." : map ("  " <>) reason2
              pure Continue

  fire nextWait0 (messages, loaded, loaded)


-- | Given an available height, and a set of messages to display, show them as best you can.
prettyOutput :: String -> Int -> [Load] -> [EvalResult] -> [String]
prettyOutput currTime loadedCount [] evals =
  (allGoodMessage <> " (" <> show loadedCount <> " module" <> ['s' | loadedCount /= 1] <> ", at " <> currTime <> ")")
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
    ["  " <> (if j == 0 then "[" else ",") <> b | (j,b) <- List.zipFrom (0 :: Int) bs] <>
    [if null bs then "  []" else "  ]"]
  | (i,(a,bs)) <- List.zipFrom (0 :: Int) xs] <>
  [["}"]]

jString :: String -> String
jString x = "\"" <> List.escapeJSON x <> "\""

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
