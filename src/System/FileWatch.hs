{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : System.FileWatch
-- Copyright   : (c) mingmingrr, 2024
-- License     : MIT
-- Maintainer  : mingmingrr@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Watch a directory and run commands on file changes.

module System.FileWatch
  ( watch
  -- * Types
  , FileWatchConfig(..)
  , FileWatchState(..)
  , Shell(..)
  , ProcHandler
  , EditProcess
  -- * Shell actions
  , runShell
  , section
  , stage
  , putSingleHeader
  , resetFailure
  -- * Running commands
  , shellIO
  , shellCV
  , shellCS
  , shellPV
  , shellPS
  , shellCIV
  , shellCIS
  , shellPIV
  , shellPIS
  , shellCH
  , shellCEH
  , shellPH
  , shellPEH
  -- * Helpers
  , withEnv
  , askState
  ) where

import Optics
import Text.Read (readMaybe)
import Debug.Trace

import Data.Time
import Data.Default
import Data.Maybe
import Data.Foldable
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as UTF8

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Catch
import qualified Control.Exception as Exc

import System.IO
import System.INotify
import System.Directory
import System.FilePath
import System.Environment
import qualified System.Console.ANSI as ANSI
import qualified System.Console.ANSI.Types as ANSI
import System.Process hiding (Shell)
import qualified System.Process as Process
import System.Posix.Process (executeFile)
import qualified System.Console.Terminal.Size as Size


-- | Shell operations with an attached `FileWatchConfig`
newtype Shell a = Shell
  { getShell :: ReaderT FileWatchConfig IO a }
  deriving ( Functor, Applicative
    , Monad, MonadIO, MonadFix
    , MonadThrow, MonadCatch, MonadMask
    , MonadReader FileWatchConfig )

-- | Mutable file watcher state
data FileWatchState = FileWatchState
  -- | Currently running `Shell` thread
  { threadRunner :: Maybe ThreadId
  -- | Stage where previous run failed
  , failAtStage :: Int
  -- | Current stage number
  , currentStage :: Int
  -- | Inotify watch descriptors and files in directory
  , inotifyFDs   :: Map FilePath (WatchDescriptor, Set FilePath)
  }

-- | Read only file watch configuration and mutable state
data FileWatchConfig = FileWatchConfig
  -- | Skip successful sections on failure
  { startFromFail :: Bool
  -- | Clear the screen on (re)start
  , clearOnStart  :: Bool
  -- | Recursively add watch directories
  , recursive     :: Bool
  -- | Print debug events
  , verbose       :: Bool
  -- | Action after watches have been added
  , initAction    :: Shell ()
  -- | Action when the file `programPath` changes
  , restartAction :: Shell ()
  -- | Filter watched file changes
  , filterFile    :: FilePath -> Shell Bool
  -- | Action when filtered files change
  , runAction     :: FilePath -> Shell ()
  -- | Files/directories to watch for changes
  , watchFiles    :: Set FilePath
  -- | Print time on (re)start,
  -- see `Data.Time.Format.formatTime`
  , timeFormat    :: Maybe String
  -- | Style for stage headers,
  , putHeader     :: String -> Shell ()
  -- | Path to the current watch program,
  -- set this to @\_\_FILE\_\_@
  , programPath   :: FilePath
  -- | /internal/ - inotify handle
  , inotifyRoot   :: INotify
  -- | /internal/ - current watch state
  , watchState    :: MVar FileWatchState
  }

makeFieldLabelsNoPrefix ''FileWatchState
makeFieldLabelsNoPrefix ''FileWatchConfig

instance Default FileWatchState where
  def = FileWatchState
    { threadRunner = Nothing
    , failAtStage  = 0
    , currentStage = 0
    , inotifyFDs   = Map.empty
    }

instance Default FileWatchConfig where
  def = FileWatchConfig
    { startFromFail = True
    , clearOnStart  = True
    , recursive     = True
    , verbose       = True
    , initAction    = return ()
    , restartAction = defaultRestartAction
    , filterFile    = \_ -> return True
    , runAction     = \_ -> return ()
    , watchFiles    = Set.singleton "."
    , timeFormat    = Just "%T"
    , putHeader     = defaultPutHeader
    , programPath   = error "programPath not set"
    , inotifyRoot   = error "inotifyRoot not set"
    , watchState    = error "watchState not set"
    }

defaultPutHeader :: String -> Shell ()
defaultPutHeader text = liftIO $ do
  ANSI.setSGR
    [ ANSI.Reset
    , ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Cyan
    , ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.White ]
  putStr "\9608\9608\57520"
  ANSI.setSGR
    [ ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Black
    , ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.White ]
  putChar ' '
  putStr text
  putChar ' '
  ANSI.clearFromCursorToLineEnd
  putStrLn ""
  ANSI.setSGR [ ANSI.Reset ]
  return ()

defaultRestartAction :: Shell ()
defaultRestartAction = do
  killThreadRunner
  cleanupWatchDescs
  path <- asks programPath
  liftIO (executeFile path True [] Nothing)

termWidth :: MonadIO m => m Int
termWidth = liftIO . fmap (either id id) . runExceptT $ do
  liftIO Size.size >>= \case
    Just (Size.Window{Size.width=n}) -> throwError n
    _ -> return ()
  liftIO (Exc.try @Exc.SomeException (readProcess "stty" ["size"] "")) >>= \case
    Right (words -> [_, readMaybe -> Just n]) -> throwError n
    _ -> return ()
  liftIO (Exc.try @Exc.SomeException (readProcess "tput" ["cols"] "")) >>= \case
    Right (readMaybe -> Just n) -> throwError n
    _ -> return ()
  liftIO (lookupEnv "WIDTH") >>= \case
    Just (readMaybe -> Just n) -> throwError n
    _ -> return ()
  return 80

-- | Custom process handler function, see `System.Process.createProcess`
type ProcHandler r
  -- | (stdin, stdout, stderr, ph)
  = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
  -> IO r

-- | Edit a `System.Process.CreateProcess` before creating the process
type EditProcess = CreateProcess -> CreateProcess

putSectionHeader :: String -> Shell ()
putSectionHeader header = ask >>= \conf@FileWatchConfig{..} -> do
  liftIO $ putStr "\x1b]133;A\a"
  putHeader header
  liftIO $ do
    putStr "\x1b]133;B\a"
    putStr "\x1b]133;C\a"
    hFlush stdout

putSectionFooter :: MonadIO m => m ()
putSectionFooter = liftIO $ putStr "\x1b]133;D;0\a" >> hFlush stdout

-- | Run a `Shell` action
runShell :: Shell a -> FileWatchConfig -> IO a
runShell = runReaderT . getShell

asProcHandler :: ProcHandler a -> Maybe Handle
  -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a
asProcHandler fn stdin stdout stderr ph = fn (stdin, stdout, stderr, ph)

-- | Run an `IO` action in a `Shell`
shellIO :: MonadIO m => IO r -> m r
shellIO = liftIO
-- | Run a shell __C__ommand and return __V__oid,
-- see `System.Process.callCommand`
shellCV :: MonadIO m => String -> m ()
shellCV cmd = liftIO $ callCommand cmd
-- | Run a shell __C__ommand and return stdout __S__tring,
-- see `System.Process.callCommand`
shellCS :: MonadIO m => String -> m String
shellCS cmd = liftIO $ readCreateProcess (Process.shell cmd) ""
-- | Run a __P__rocess and return __V__oid,
-- see `System.Process.callProcess`
shellPV :: MonadIO m => String -> [String] -> m ()
shellPV cmd args = liftIO $ callProcess cmd args
-- | Run a __P__rocess and return stdout __S__tring,
-- see `System.Process.callProcess`
shellPS :: MonadIO m => String -> [String] -> m String
shellPS cmd args = liftIO $ readProcess cmd args ""
-- | Run a shell __C__ommand with __I__nput and return __V__oid
shellCIV :: MonadIO m => String -> String -> m ()
shellCIV cmd input = liftIO $ void $ readCreateProcess (Process.shell cmd) input
-- | Run a shell __C__ommand with __I__nput and return stdout __S__tring
shellCIS :: MonadIO m => String -> String -> m String
shellCIS cmd input = liftIO $ readCreateProcess (Process.shell cmd) input
-- | Run a __P__rocess with __I__nput and return __V__oid
shellPIV :: MonadIO m => String -> [String] -> String -> m ()
shellPIV cmd args input = liftIO $ void $ readProcess cmd args input
-- | Run a __P__rocess with __I__nput and return stdout __S__tring
shellPIS :: MonadIO m => String -> [String] -> String -> m String
shellPIS cmd args input = liftIO $ readProcess cmd args input
-- | Run a shell __C__ommand with a __H__andler,
-- see `System.Process.withCreateProcess`
shellCH :: MonadIO m => String -> ProcHandler r -> m r
shellCH cmd fn = liftIO $ withCreateProcess (Process.shell cmd) (asProcHandler fn)
-- | Run a __E__dited shell __C__ommand with a __H__andler,
-- see `System.Process.withCreateProcess`
shellCEH :: MonadIO m => String -> EditProcess -> ProcHandler r -> m r
shellCEH cmd edit fn = liftIO $ withCreateProcess (edit (Process.shell cmd)) (asProcHandler fn)
-- | Run a __P__rocess with a __H__andler,
-- see `System.Process.withCreateProcess`
shellPH :: MonadIO m => String -> [String] -> ProcHandler r -> m r
shellPH cmd args fn = liftIO $ withCreateProcess (proc cmd args) (asProcHandler fn)
-- | Run a __E__dited __P__rocess with a __H__andler,
-- see `System.Process.withCreateProcess`
shellPEH :: MonadIO m => String -> [String] -> EditProcess -> ProcHandler r -> m r
shellPEH cmd args edit fn = liftIO $ withCreateProcess (edit (proc cmd args)) (asProcHandler fn)

-- | Run a `Shell` action with a header at the top
--
-- Gets skipped if `startFromFail` is `True`,
-- and previous run was successful.
section :: String -> Shell () -> Shell ()
section = stageInc 0

-- | Run a `Shell` action with a header at the top,
-- and sets a failure checkpoint
--
-- Gets skipped if `startFromFail` is `True`,
-- and previous run was successful.
stage :: String -> Shell () -> Shell ()
stage = stageInc 1

stageInc :: Int -> String -> Shell () -> Shell ()
stageInc inc header action = do
  conf@FileWatchConfig{..} <- ask
  FileWatchState{..} <- liftIO $ readMVar watchState
  unless (startFromFail && (currentStage < failAtStage))
    $ bracket_ (putSectionHeader header) putSectionFooter action
  unless (inc == 0) $ liftIO $ modifyMVar_ watchState
    $ return . (#currentStage %~ (+ inc))

-- | Reset any failure checkpoints from the previous run
resetFailure :: Shell ()
resetFailure = ask >>= \conf@FileWatchConfig{..} ->
  liftIO $ modifyMVar_ watchState $ return . (#failAtStage .~ 0)

-- | Print a stage header
putSingleHeader :: String -> Shell ()
putSingleHeader header = putSectionHeader header >> putSectionFooter

handleEvent :: FilePath -> Event -> Shell ()
handleEvent path Closed{isDirectory=False, maybeFilePath=file, wasWriteable=True}
  = handleWriteEvent (maybe path ((path </>) . BS.unpack) file)
handleEvent path Created{isDirectory=True, filePath=file}
  = addWatchPath (path </> BS.unpack file)
handleEvent path DeletedSelf
  = deleteWatchPath path
handleEvent path Deleted{isDirectory=True, filePath=file}
  = deleteWatchPath (path </> BS.unpack file)
handleEvent path MovedSelf{isDirectory=True}
  = deleteWatchPath path
handleEvent path MovedOut{isDirectory=True, filePath=file}
  = deleteWatchPath (path </> BS.unpack file)
handleEvent path MovedIn{isDirectory=True, filePath=file}
  = addWatchPath (path </> BS.unpack file)
handleEvent _ event = asks verbose >>= \v ->
  when v $ putSingleHeader $ "event: " ++ show event

killThreadRunner :: Shell ()
killThreadRunner = ask >>= \conf@FileWatchConfig{..} -> liftIO $ do
  thread <- modifyMVar watchState $ \s -> return
    ( #threadRunner .~ Nothing $ #currentStage .~ 0
    $ #failAtStage .~ view #currentStage s $ s , threadRunner s)
  traverse_ (\t -> killThread t >> threadDelay 50000) thread

handleWriteEvent :: FilePath -> Shell ()
handleWriteEvent file = do
  conf@FileWatchConfig{..} <- ask
  when (file == programPath) restartAction
  filt <- if file `Set.member` watchFiles
    then return True else filterFile file
  when filt $ do
    killThreadRunner
    thread <- liftIO $ forkIO $ runShell (runThreadRunner file) conf
    liftIO $ modifyMVar_ watchState $ return . (#threadRunner ?~ thread)

runThreadRunner :: FilePath -> Shell ()
runThreadRunner file = do
  conf@FileWatchConfig{..} <- ask
  when clearOnStart $ liftIO $ callProcess "tput" ["reset"]
  forM_ timeFormat $ \fmt -> do
    time <- liftIO getZonedTime
    putSingleHeader $ formatTime defaultTimeLocale fmt time
  result <- liftIO $ try $ runShell (runAction file) conf
  section "done" (return ())
  case result of
    Left exc -> putSingleHeader $ "exception: " ++ show (exc :: SomeException)
    Right _ -> liftIO $ modifyMVar_ watchState $ return . (#currentStage .~ 0)

cleanupWatchDescs :: Shell ()
cleanupWatchDescs = do
  conf@FileWatchConfig{..} <- ask
  FileWatchState{..} <- liftIO (readMVar watchState)
  traverse_ (liftIO . removeWatch . fst) inotifyFDs

addWatchPath :: FilePath -> Shell ()
addWatchPath relpath = do
  conf@FileWatchConfig{..} <- ask
  path <- liftIO (makeAbsolute relpath)
  let watchEvents = [CloseWrite, Move, MoveSelf, Create, Delete, DeleteSelf]
  desc <- liftIO $ addWatch inotifyRoot watchEvents (UTF8.fromString path)
    $ \event -> runShell (handleEvent path event) conf
  files <- addWatchDir path
  liftIO $ modifyMVar_ watchState $ return .
    (#inotifyFDs %~ Map.insert path (desc, Set.fromList files))
  return ()

addWatchDir :: FilePath -> Shell [FilePath]
addWatchDir path = do
  conf@FileWatchConfig{..} <- ask
  dir <- liftIO (doesDirectoryExist path)
  if not (dir && recursive) then return [] else do
    files <- map (path </>) <$> liftIO (listDirectory path)
    fmap catMaybes $ forM files $ \file -> do
      dir <- liftIO (doesDirectoryExist file)
      if dir then Just file <$ addWatchPath file else return Nothing

deleteWatchPath :: FilePath -> Shell ()
deleteWatchPath relpath = do
  conf@FileWatchConfig{..} <- ask
  path <- liftIO (makeAbsolute relpath)
  unless (path `Set.member` watchFiles) $ do
    files <- liftIO $ modifyMVar watchState $ \s ->
      let (m, xs) = Map.alterF (, Nothing) path (inotifyFDs s)
       in return (#inotifyFDs .~ xs $ s, m)
    traverse_ (liftIO . removeWatch . fst) files
    traverse_ (mapM_ deleteWatchPath . snd) files

-- | Entry point for file watch,
-- see `FileWatchConfig` for options
--
-- __Note__: set `programPath` to @\_\_FILE\_\_@
--
-- Uses libinotify to watch for file changes,
-- and runs `runAction` on changes that are allowed
-- filtered by `filterFile` or files in `watchFiles`.
watch :: FileWatchConfig -> IO ()
watch conf = withINotify $ \inotify -> do
  state <- newMVar def
  files@(prog : _) <- mapM makeAbsolute
    $ view #programPath conf
    : toListOf (#watchFiles % folded) conf
  let conf'@FileWatchConfig{..} = conf
        { inotifyRoot = inotify
        , watchState = state
        , programPath = prog
        , watchFiles = Set.fromList files
        }
  flip runShell conf' $ do
    when verbose $ putSingleHeader "setup inotify"
    mapM_ addWatchPath watchFiles
    when verbose $ putSingleHeader "init action"
    initAction
  forever $ threadDelay 10000000

-- | Run an action with modified environment variables
withEnv :: [(String, Maybe String)] -> Shell a -> Shell a
withEnv env action = do
  conf <- ask
  let names = map fst env
      save = mapM lookupEnv names
      restore = zipWithM_ setMaybe names
      setMaybe key val = maybe unsetEnv (flip setEnv) val key
  liftIO $ bracket save restore (const (runShell action conf))

-- | Read the current file watch state
askState :: Shell FileWatchState
askState = asks watchState >>= liftIO . readMVar
