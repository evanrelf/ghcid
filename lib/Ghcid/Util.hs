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
  , getModTimeResolution
  , getShortTime
  )
where

import Data.Time.Clock (UTCTime)
import System.FilePath ((</>))

import qualified Control.Concurrent.Extra as Concurrent
import qualified Control.Exception as Exception
import qualified Control.Monad.Extra as Monad
import qualified Data.List as List
import qualified Data.Time as Time
import qualified Data.Version as Version
import qualified System.Console.ANSI as Ansi
import qualified System.Directory as Directory
import qualified System.IO.Error as IO.Error
import qualified System.IO.Extra as IO
import qualified System.IO.Unsafe as IO.Unsafe
import qualified System.Info
import qualified System.Time.Extra as System.Time

-- | Flags that are required for ghcid to function and are supported on all GHC
--   versions
ghciFlagsRequired :: [String]
ghciFlagsRequired =
    ["-fno-break-on-exception","-fno-break-on-error" -- see #43
    ,"-v1" -- see #110
    ]

-- | Flags that are required for ghcid to function, but are only supported on
--   some GHC versions
ghciFlagsRequiredVersioned :: [String]
ghciFlagsRequiredVersioned =
    ["-fno-hide-source-paths" -- see #132, GHC 8.2 and above
    ]

-- | Flags that make ghcid work better and are supported on all GHC versions
ghciFlagsUseful :: [String]
ghciFlagsUseful =
    ["-ferror-spans" -- see #148
    ,"-j" -- see #153, GHC 7.8 and above, but that's all I support anyway
    ]

-- | Flags that make ghcid work better, but are only supported on some GHC
--   versions
ghciFlagsUsefulVersioned :: [String]
ghciFlagsUsefulVersioned =
    ["-fdiagnostics-color=always" -- see #144, GHC 8.2 and above
    ]


-- | Drop a prefix from a list, no matter how many times that prefix is present
dropPrefixRepeatedly :: Eq a => [a] -> [a] -> [a]
dropPrefixRepeatedly []  s = s
dropPrefixRepeatedly pre s =
  maybe s (dropPrefixRepeatedly pre) $ List.stripPrefix pre s


{-# NOINLINE lock #-}
lock :: Concurrent.Lock
lock = IO.Unsafe.unsafePerformIO Concurrent.newLock

-- | Output a string with some level of locking
outStr :: String -> IO ()
outStr msg = do
    evaluateWHNF_ $ length msg
    Concurrent.withLock lock $ putStr msg

outStrLn :: String -> IO ()
outStrLn xs = outStr $ xs <> "\n"

-- | Ignore all exceptions coming from an action
ignored :: IO () -> IO ()
ignored act = do
    bar <- Concurrent.newBarrier
    void $ Concurrent.forkFinally act $ const $ Concurrent.signalBarrier bar ()
    Concurrent.waitBarrier bar

-- | The message to show when no errors have been reported
allGoodMessage :: String
allGoodMessage = green <> "All good" <> reset
  where
    green = Ansi.setSGRCode [Ansi.SetColor Ansi.Foreground Ansi.Dull Ansi.Green]
    reset = Ansi.setSGRCode [Ansi.Reset]

-- | Given a 'FilePath' return either 'Nothing' (file does not exist) or 'Just'
--   (the modification time)
getModTime :: FilePath -> IO (Maybe UTCTime)
getModTime file = Exception.handleJust
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


-- | Get the smallest difference that can be reported by two modification times
getModTimeResolution :: IO System.Time.Seconds
getModTimeResolution = pure getModTimeResolutionCache

{-# NOINLINE getModTimeResolutionCache #-}
-- Cache the result so only computed once per run
getModTimeResolutionCache :: System.Time.Seconds
getModTimeResolutionCache = IO.Unsafe.unsafePerformIO $ IO.withTempDir \dir -> do
    let file = dir </> "calibrate.txt"

    -- with 10 measurements can get a bit slow, see Shake issue tracker #451
    -- if it rounds to a second then 1st will be a fraction, but 2nd will be full
    -- second
    mtime <- do
      mtime <- List.maximum <$> forM [1..3 :: Int] \i -> fst <$> System.Time.duration do
        writeFile file $ show i
        t1 <- Directory.getModificationTime file
        flip Monad.loopM (0 :: Int) \j -> do
            writeFile file $ show (i,j)
            t2 <- Directory.getModificationTime file
            pure $ if t1 == t2 then Left $ j+1 else Right ()

      -- GHC 7.6 and below only have 1 sec resolution timestamps
      pure $ if System.Info.compilerVersion < Version.makeVersion [7,8] then max mtime 1 else mtime

    putStrLn $ "Longest file modification time lag was " <> show (ceiling (mtime * 1000) :: Integer) <> "ms"
    -- add a little bit of safety, but if it's really quick, don't make it that
    -- much slower
    pure $ mtime + min 0.1 mtime
