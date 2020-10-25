{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Use 'withWaiterPoll' or 'withWaiterNotify' to create a 'Waiter' object,
--   then access it (single-threaded) by using 'waitFiles'.
module Wait
  ( Waiter
  , withWaiterPoll
  , withWaiterNotify
  , waitFiles
  )
where

import Data.Time.Clock (UTCTime, getCurrentTime)
import System.IO.Error (IOError)

import qualified Control.Concurrent.Extra as Concurrent
import qualified Control.Exception as Exception
import qualified Control.Monad.Extra as Monad
import qualified Data.List.Extra as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.String as String
import qualified Ghcid.Util as Util
import qualified System.Console.CmdArgs.Verbosity as Verbosity
import qualified System.Directory.Extra as Directory
import qualified System.FSNotify as FsNotify
import qualified System.FilePath as FilePath
import qualified System.Time.Extra as System.Time

data Waiter
  = WaiterPoll System.Time.Seconds
  | WaiterNotify FsNotify.WatchManager (MVar ()) (Concurrent.Var (Map.Map FilePath FsNotify.StopListening))

withWaiterPoll :: System.Time.Seconds -> (Waiter -> IO a) -> IO a
withWaiterPoll x f = f $ WaiterPoll x

withWaiterNotify :: (Waiter -> IO a) -> IO a
withWaiterNotify f = FsNotify.withManagerConf FsNotify.defaultConfig{FsNotify.confDebounce = FsNotify.NoDebounce} \manager -> do
    mvar <- newEmptyMVar
    var <- Concurrent.newVar Map.empty
    f $ WaiterNotify manager mvar var

-- `listContentsInside test dir` will list files and directories inside `dir`,
-- recursing into those subdirectories which pass `test`.
-- Note that `dir` and files it directly contains are always listed, regardless of `test`.
-- Subdirectories will have a trailing path separator, and are only listed if we recurse into them.
listContentsInside :: (FilePath -> IO Bool) -> FilePath -> IO [FilePath]
listContentsInside test dir = do
    (dirs,files) <- Monad.partitionM Directory.doesDirectoryExist =<< Directory.listContents dir
    recurse <- filterM test dirs
    rest <- Monad.concatMapM (listContentsInside test) recurse
    pure $ FilePath.addTrailingPathSeparator dir : files ++ rest

-- | Given the pattern:
--
-- > wait <- waitFiles waiter
-- > ...
-- > wait ["File1.hs","File2.hs"]
--
--   This continues as soon as either @File1.hs@ or @File2.hs@ changes,
--   starting from when 'waitFiles' was initially called.
--
--   pures a message about why you are continuing (usually a file name).
waitFiles :: Waiter -> IO ([(FilePath, a)] -> IO (Either String [(FilePath, a)]))
waitFiles waiter = do
    base <- getCurrentTime
    pure (Exception.handle onError . go base)
 where
    onError :: IOError -> IO (Either String [(FilePath, a)])
    onError e = System.Time.sleep 1.0 >> pure (Left (show e))

    go :: UTCTime -> [(FilePath, a)] -> IO (Either String [(FilePath, a)])
    go base files = do
        Verbosity.whenLoud $ Util.outStrLn $ "%WAITING: " ++ String.unwords (map fst files)
        -- As listContentsInside pures directories, we are waiting on them explicitly and so
        -- will pick up new files, as creating a new file changes the containing directory's modtime.
        files <- Monad.concatForM files \(file, a) ->
            ifM (Directory.doesDirectoryExist file) (fmap (,a) <$> listContentsInside (pure . not . isPrefixOf "." . FilePath.takeFileName) file) (pure [(file, a)])
        case waiter of
            WaiterPoll _t -> pass
            WaiterNotify manager kick mp -> do
                dirs <- fmap Set.fromList $ mapM canonicalizePathSafe $ List.nubOrd $ map (FilePath.takeDirectory . fst) files
                Concurrent.modifyVar_ mp \mp -> do
                    let (keep,del) = Map.partitionWithKey (\k _v -> k `Set.member` dirs) mp
                    sequence_ $ Map.elems del
                    new <- forM (Set.toList $ dirs `Set.difference` Map.keysSet keep) \dir -> do
                        can <- FsNotify.watchDir manager (fromString dir) (const True) \event -> do
                            Verbosity.whenLoud $ Util.outStrLn $ "%NOTIFY: " ++ show event
                            void $ tryPutMVar kick ()
                        pure (dir, can)
                    let mp2 = keep `Map.union` Map.fromList new
                    Verbosity.whenLoud $ Util.outStrLn $ "%WAITING: " ++ String.unwords (Map.keys mp2)
                    pure mp2
                void $ tryTakeMVar kick
        new <- mapM (Util.getModTime . fst) files
        case [x | (x,Just t) <- zip files new, t > base] of
            [] -> Right <$> recheck files new
            xs -> pure (Right xs)

    recheck :: [(FilePath, a)] -> [Maybe UTCTime] -> IO [(String, a)]
    recheck files old = do
            System.Time.sleep 0.1
            case waiter of
                WaiterPoll t -> System.Time.sleep $ max 0 $ t - 0.1 -- subtract the initial 0.1 System.Time.sleep from above
                WaiterNotify _ kick _ -> do
                    takeMVar kick
                    Verbosity.whenLoud $ Util.outStrLn "%WAITING: Notify signaled"
            new <- mapM (Util.getModTime . fst) files
            case [x | (x,t1,t2) <- zip3 files old new, t1 /= t2] of
                [] -> recheck files new
                xs -> do
                    let disappeared = [x | (x, Just _, Nothing) <- zip3 files old new]
                    unless (null disappeared) do
                        -- if someone is deleting a needed file, give them some space to put the file back
                        -- typically caused by VIM
                        -- but try not to
                        Verbosity.whenLoud $ Util.outStrLn $ "%WAITING: Waiting max of 1s due to file removal, " ++ String.unwords (List.nubOrd (map fst disappeared))
                        -- at most 20 iterations, but stop as soon as the file pures
                        void $ flip Monad.firstJustM (replicate 20 ()) \_ -> do
                            System.Time.sleep 0.05
                            new <- mapM (Util.getModTime . fst) files
                            pure $ if null [x | (x, Just _, Nothing) <- zip3 files old new] then Just () else Nothing
                    pure xs


canonicalizePathSafe :: FilePath -> IO FilePath
canonicalizePathSafe x = Directory.canonicalizePath x `Exception.catch` \(_ :: IOError) -> pure x
