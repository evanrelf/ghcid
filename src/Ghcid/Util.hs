-- | Utility functions
module Ghcid.Util
  ( ghciFlagsRequired
  , ghciFlagsRequiredVersioned
  , ghciFlagsUseful
  , ghciFlagsUsefulVersioned
  , dropPrefixRepeatedly
  , takeRemainder
  , outStr
  , outStrLn
  , ignored
  , allGoodMessage
  , getModTime
  , getShortTime
  )
where

import Data.Time.Clock (UTCTime)

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Extra as Concurrent.Extra
import qualified Control.Exception as Exception
import qualified Data.List as List
import qualified Data.Time as Time
import qualified System.Console.ANSI as Ansi
import qualified System.Directory as Directory
import qualified System.IO.Error as IO.Error
import qualified System.IO.Unsafe as IO.Unsafe

-- | Flags that are required for ghcid to function and are supported on all GHC
--   versions
ghciFlagsRequired :: [String]
ghciFlagsRequired =
  [ "-fno-break-on-exception","-fno-break-on-error" -- see #43
  , "-v1" -- see #110
  ]

-- | Flags that are required for ghcid to function, but are only supported on
--   some GHC versions
ghciFlagsRequiredVersioned :: [String]
ghciFlagsRequiredVersioned =
  [ "-fno-hide-source-paths" -- see #132, GHC 8.2 and above
  ]

-- | Flags that make ghcid work better and are supported on all GHC versions
ghciFlagsUseful :: [String]
ghciFlagsUseful =
  [ "-ferror-spans" -- see #148
  , "-j" -- see #153, GHC 7.8 and above, but that's all I support anyway
  ]

-- | Flags that make ghcid work better, but are only supported on some GHC
--   versions
ghciFlagsUsefulVersioned :: [String]
ghciFlagsUsefulVersioned =
  [ "-fdiagnostics-color=always" -- see #144, GHC 8.2 and above
  ]


-- | Drop a prefix from a list, no matter how many times that prefix is present
dropPrefixRepeatedly :: Eq a => [a] -> [a] -> [a]
dropPrefixRepeatedly [] xs = xs
dropPrefixRepeatedly prefix xs =
  maybe xs (dropPrefixRepeatedly prefix) (List.stripPrefix prefix xs)


{-# NOINLINE lock #-}
lock :: Concurrent.Extra.Lock
lock = IO.Unsafe.unsafePerformIO Concurrent.Extra.newLock

-- | Output a string with some level of locking
outStr :: String -> IO ()
outStr msg = do
  evaluateWHNF_ $ length msg
  Concurrent.Extra.withLock lock $ putStr msg

outStrLn :: String -> IO ()
outStrLn xs = outStr $ xs <> "\n"

-- | Ignore all exceptions coming from an action
ignored :: IO () -> IO ()
ignored action = do
  barrier <- Concurrent.Extra.newBarrier
  void $ Concurrent.forkFinally action \_ -> Concurrent.Extra.signalBarrier barrier ()
  Concurrent.Extra.waitBarrier barrier

-- | The message to show when no errors have been reported
allGoodMessage :: String
allGoodMessage = green <> "All good" <> reset
  where
    green = Ansi.setSGRCode [Ansi.SetColor Ansi.Foreground Ansi.Dull Ansi.Green]
    reset = Ansi.setSGRCode [Ansi.Reset]

-- | Given a 'FilePath' return either 'Nothing' (file does not exist) or 'Just'
--   (the modification time)
getModTime :: FilePath -> IO (Maybe UTCTime)
getModTime file =
  Exception.handleJust
    (\e -> if IO.Error.isDoesNotExistError e then Just () else Nothing)
    (\_ -> pure Nothing)
    (Just <$> Directory.getModificationTime file)

-- | Returns both the amount left (could have been taken more) and the list
takeRemainder :: Int -> [a] -> (Int, [a])
takeRemainder n xs = let ys = take n xs in (n - length ys, ys)

-- | Get the current time in the current timezone in HH:MM:SS format
getShortTime :: IO String
getShortTime =
  Time.formatTime Time.defaultTimeLocale "%H:%M:%S" <$> Time.getZonedTime
