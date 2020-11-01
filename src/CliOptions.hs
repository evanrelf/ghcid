{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CliOptions
  ( ColorMode (..)
  , CliOptions (..)
  , getCliOptions
  )
where

import qualified System.Time.Extra as System.Time
import qualified Options.Applicative as Options


-- | When to colour terminal output.
data ColorMode
  = Never  -- ^ Terminal output will never be coloured.
  | Always -- ^ Terminal output will always be coloured.
  | Auto   -- ^ Terminal output will be coloured if $TERM and stdout appear to support it.


parseColorMode :: String -> Maybe ColorMode
parseColorMode = \case
  "never" -> Just Never
  "always" -> Just Always
  "auto" -> Just Auto
  _ -> Nothing


-- | Command line options
data CliOptions = CliOptions
  { command :: String
  , arguments :: [String]
  , test :: [String]
  , test_message :: String
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
  -- TODO @evan: What to do about these?
  -- - `verbose`
  -- - `quiet`
  -- - `version`
  -- - `numeric-version`
  }


getCliOptions :: MonadIO m => m CliOptions
getCliOptions = liftIO do
  let parserPrefs = Options.prefs Options.showHelpOnError
  let parserInfo = Options.info (Options.helper <*> parseCliOptions) $ mconcat
        [ Options.progDesc "Auto reloading GHCi daemon"
        ]
  Options.customExecParser parserPrefs parserInfo


parseCliOptions :: Options.Parser CliOptions
parseCliOptions = do
  command :: String <-
    Options.strOption $ mconcat
      [ Options.long "command"
      , Options.short 'c'
      , Options.metavar "COMMAND"
      , Options.help "Command to run (defaults to ghci or cabal repl)"
      , Options.value ""
      ]

  arguments :: [String] <-
    many $ Options.strArgument $ mconcat
      [ Options.metavar "MODULE"
      ]

  test :: [String] <-
    many $ Options.strOption $ mconcat
      [ Options.long "test"
      , Options.short 'T'
      , Options.metavar "EXPR"
      , Options.help "Command to run after successful loading"
      ]

  test_message :: String <-
    Options.strOption $ mconcat
      [ Options.long "test-message"
      , Options.metavar "MESSAGE"
      , Options.help "Message to show before running the test (defaults to \"Running test...\")"
      , Options.value "Running test..."
      , Options.showDefault
      ]

  warnings :: Bool <-
    Options.switch $ mconcat
      [ Options.long "warnings"
      , Options.short 'W'
      , Options.help "Allow tests to run even with warnings"
      ]

  lint :: Maybe String <-
    Options.optional $ Options.strOption $ mconcat
      [ Options.long "lint"
      , Options.short 'l'
      , Options.metavar "COMMAND"
      , Options.help "Linter to run if there are no errors"
      ]

  no_status :: Bool <-
    Options.switch $ mconcat
      [ Options.long "no-status"
      , Options.short 'S'
      , Options.help "Suppress status messages"
      ]

  clear :: Bool <-
    Options.switch $ mconcat
      [ Options.long "clear"
      , Options.help "Clear screen when reloading"
      ]

  reverse_errors :: Bool <-
    Options.switch $ mconcat
      [ Options.long "reverse-errors"
      , Options.help "Reverse output order (works best with --no-height-limit)"
      ]

  no_height_limit :: Bool <-
    Options.switch $ mconcat
      [ Options.long "no-height-limit"
      , Options.help "Disable height limit"
      ]

  height :: Maybe Int <-
    Options.optional $ Options.option Options.auto $ mconcat
      [ Options.long "height"
      , Options.short 'h'
      , Options.metavar "INT"
      , Options.help "Number of lines to use (defaults to console height)"
      ]

  width :: Maybe Int <-
    Options.optional $ Options.option Options.auto $ mconcat
      [ Options.long "width"
      , Options.short 'w'
      , Options.metavar "INT"
      , Options.help "Number of columns to use (defaults to console width)"
      ]

  no_title :: Bool <-
    Options.switch $ mconcat
      [ Options.long "no-title"
      , Options.help "Don't update the shell title/icon"
      ]

  project :: String <-
    Options.strOption $ mconcat
      [ Options.long "project"
      , Options.short 'p'
      , Options.metavar "NAME"
      , Options.help "Name of the project, defaults to current directory"
      , Options.value ""
      ]

  reload :: [String] <-
    many $ Options.strOption $ mconcat
      [ Options.long "reload"
      , Options.metavar "PATH"
      , Options.help "Reload when the given file or directory contents change (defaults to none)"
      ]

  restart :: [String] <-
    many $ Options.strOption $ mconcat
      [ Options.long "restart"
      , Options.metavar "PATH"
      , Options.help "Restart the command when the given file or directory contents change (defaults to .ghci and any .cabal file, unless when using stack or a custom command)"
      ]

  directory :: String <-
    Options.strOption $ mconcat
      [ Options.long "directory"
      , Options.short 'C'
      , Options.metavar "DIR"
      , Options.help "Set the current directory"
      , Options.value "."
      ]

  outputfile :: [String] <-
    many $ Options.strOption $ mconcat
      [ Options.long "outputfile"
      , Options.short 'o'
      , Options.metavar "FILE"
      , Options.help "File to write the full output to"
      ]

  ignoreLoaded :: Bool <-
    Options.switch $ mconcat
      [ Options.long "ignore-loaded"
      , Options.help "Keep going if no files are loaded. Requires --reload to be set."
      ]

  poll :: Maybe System.Time.Seconds <-
    Options.optional $ Options.option Options.auto $ mconcat
      [ Options.long "poll"
      , Options.metavar "SECONDS"
      , Options.help "Use polling every N seconds (defaults to using notifiers)"
      ]

  max_messages :: Maybe Int <-
    Options.optional $ Options.option Options.auto $ mconcat
      [ Options.long "max-messages"
      , Options.short 'n'
      , Options.metavar "INT"
      , Options.help "Maximum number of messages to print"
      ]

  color :: ColorMode <-
    Options.option (Options.maybeReader parseColorMode) $ mconcat
      [ Options.long "color"
      , Options.metavar "always/never/auto"
      , Options.help "Color output (defaults to when the terminal supports it)"
      , Options.value Auto
      ]

  setup :: [String] <-
    many $ Options.strOption $ mconcat
      [ Options.long "setup"
      , Options.short 's'
      , Options.metavar "COMMAND"
      , Options.help "Setup commands to pass to ghci on stdin, usually :set <something>"
      ]

  allow_eval :: Bool <-
    Options.switch $ mconcat
      [ Options.long "allow-eval"
      , Options.short 'a'
      , Options.help "Execute REPL commands in comments"
      ]

  target :: [String] <-
    many $ Options.strOption $ mconcat
      [ Options.long "target"
      , Options.metavar "TARGET"
      , Options.help "Target Component to build (e.g. lib:foo for Cabal, foo:lib for Stack)"
      ]

  pure CliOptions{..}
