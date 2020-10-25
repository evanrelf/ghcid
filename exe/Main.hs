{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- | The application entry point
module Main (main) where

import Data.Data (Data)
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
import System.Console.CmdArgs (CmdArgs, Verbosity (..), (&=))
import System.FilePath ((</>))

import qualified Control.Exception as Exception
import qualified Control.Monad.Extra as Monad
import qualified Data.List.Extra as List
import qualified Data.String as String
import qualified Data.Tuple.Extra as Tuple
import qualified Data.Version as Version
import qualified Ghcid.Escape as Escape
import qualified Ghcid.Util as Util
import qualified Session
import qualified System.Console.ANSI as Ansi
import qualified System.Console.CmdArgs as CmdArgs
import qualified System.Console.CmdArgs.Explicit as CmdArgs.Explicit
import qualified System.Console.CmdArgs.Verbosity as Verbosity
import qualified System.Console.Terminal.Size as Term
import qualified System.Directory.Extra as Directory
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified System.IO.Error as IO.Error
import qualified System.IO.Extra as IO
import qualified System.Info
import qualified System.Process as Process
import qualified System.Time.Extra as System.Time
import qualified Wait

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
  , poll :: Maybe System.Time.Seconds
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
version = Version.makeVersion [0,0]

options :: CmdArgs.Explicit.Mode (CmdArgs Options)
options = CmdArgs.cmdArgsMode $ Options
  { command = ""
      &= CmdArgs.name "c"
      &= CmdArgs.typ "COMMAND"
      &= CmdArgs.help "Command to run (defaults to ghci or cabal repl)"
  , arguments = []
      &= CmdArgs.args
      &= CmdArgs.typ "MODULE"
  , test = []
      &= CmdArgs.name "T"
      &= CmdArgs.typ "EXPR"
      &= CmdArgs.help "Command to run after successful loading"
  , test_message = "Running test..."
      &= CmdArgs.typ "MESSAGE"
      &= CmdArgs.help "Message to show before running the test (defaults to \"Running test...\")"
  , run = []
      &= CmdArgs.name "r"
      &= CmdArgs.typ "EXPR"
      &= CmdArgs.opt "main"
      &= CmdArgs.help "Command to run after successful loading (like --test but defaults to main)"
  , warnings = False
      &= CmdArgs.name "W"
      &= CmdArgs.help "Allow tests to run even with warnings"
  , lint = Nothing
      &= CmdArgs.typ "COMMAND"
      &= CmdArgs.name "lint"
      &= CmdArgs.opt "hlint"
      &= CmdArgs.help "Linter to run if there are no errors. Defaults to hlint."
  , no_status = False
      &= CmdArgs.name "S"
      &= CmdArgs.help "Suppress status messages"
  , clear = False
      &= CmdArgs.name "clear"
      &= CmdArgs.help "Clear screen when reloading"
  , reverse_errors = False
      &= CmdArgs.help "Reverse output order (works best with --no-height-limit)"
  , no_height_limit = False
      &= CmdArgs.name "no-height-limit"
      &= CmdArgs.help "Disable height limit"
  , height = Nothing
      &= CmdArgs.help "Number of lines to use (defaults to console height)"
  , width = Nothing
      &= CmdArgs.name "w"
      &= CmdArgs.help "Number of columns to use (defaults to console width)"
  , no_title = False
      &= CmdArgs.help "Don't update the shell title/icon"
  , project = ""
      &= CmdArgs.typ "NAME"
      &= CmdArgs.help "Name of the project, defaults to current directory"
  , restart = []
      &= CmdArgs.typ "PATH"
      &= CmdArgs.help "Restart the command when the given file or directory contents change (defaults to .ghci and any .cabal file, unless when using stack or a custom command)"
  , reload = []
      &= CmdArgs.typ "PATH"
      &= CmdArgs.help "Reload when the given file or directory contents change (defaults to none)"
  , directory = "."
      &= CmdArgs.typDir
      &= CmdArgs.name "C"
      &= CmdArgs.help "Set the current directory"
  , outputfile = []
      &= CmdArgs.typFile
      &= CmdArgs.name "o"
      &= CmdArgs.help "File to write the full output to"
  , ignoreLoaded = False
      &= CmdArgs.explicit
      &= CmdArgs.name "ignore-loaded"
      &= CmdArgs.help "Keep going if no files are loaded. Requires --reload to be set."
  , poll = Nothing
      &= CmdArgs.typ "SECONDS"
      &= CmdArgs.opt "0.1"
      &= CmdArgs.explicit
      &= CmdArgs.name "poll"
      &= CmdArgs.help "Use polling every N seconds (defaults to using notifiers)"
  , max_messages = Nothing
      &= CmdArgs.name "n"
      &= CmdArgs.help "Maximum number of messages to print"
  , color = Auto
      &= CmdArgs.name "colour"
      &= CmdArgs.name "color"
      &= CmdArgs.opt Always
      &= CmdArgs.typ "always/never/auto"
      &= CmdArgs.help "Color output (defaults to when the terminal supports it)"
  , setup = []
      &= CmdArgs.name "setup"
      &= CmdArgs.typ "COMMAND"
      &= CmdArgs.help "Setup commands to pass to ghci on stdin, usually :set <something>"
  , allow_eval = False
      &= CmdArgs.name "allow-eval"
      &= CmdArgs.help "Execute REPL commands in comments"
  , target = []
      &= CmdArgs.typ "TARGET"
      &= CmdArgs.help "Target Component to build (e.g. lib:foo for Cabal, foo:lib for Stack)"
  }
      &= CmdArgs.verbosity
      &=
  CmdArgs.program "ghcid"
        &= CmdArgs.summary ("Auto reloading GHCi daemon v" <> Version.showVersion version)


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
      let findStack dir = flip IO.Error.catchIOError (const $ pure Nothing) do
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
    extra <- concatMap CmdArgs.Explicit.splitArgs . String.lines <$> IO.readFile' ".ghcid"
    orig <- Environment.getArgs
    Environment.withArgs (extra <> orig) act


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

printStopped :: Options -> IO ()
printStopped opts =
  forM_ (outputfile opts) \file -> writeFile file "Ghcid has stopped.\n"


-- | Like 'main', but run with a fake terminal for testing
mainWithTerminal :: IO TermSize -> ([String] -> IO ()) -> IO ()
mainWithTerminal termSize termOutput = do
  opts <- withGhcidArgs $ CmdArgs.cmdArgsRun options
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
        opts <- pure $ opts{restart = List.nubOrd $ (origDir </> ".ghcid") : restart opts, reload = List.nubOrd $ reload opts}

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
runGhcid :: Session -> Wait.Waiter -> IO TermSize -> ([String] -> IO ()) -> Options -> IO Continue
runGhcid session waiter termSize termOutput Options{..} = do
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

  let restart = List.nubOrd $ restart <> [x | LoadConfig x <- messages]

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
          let (countErrors, countWarnings) = Tuple.both (sum :: [Int] -> Int) $ unzip
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
              let (msgError, msgWarn) = List.partition ((==) Error . loadSeverity) $ List.nubOrdOn loadMessage $ filter isMessage messages
              -- sort error messages by modtime, so newer edits cause the errors to float to the top - see #153
              errTimes <- sequence [traverseToSnd Util.getModTime x | x <- List.nubOrd $ map loadFile msgError]
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
