module Btrbkp.BkpStore.Local ( backupSnapshot
                             , createDestinationBuilder
                             , normalize
                             , rsync
                             , syncDirBuilder
                             ) where

import GHC.IO.Handle (Handle(), hClose, hGetLine, hIsEOF)

import Control.Concurrent (MVar(), forkIO, newEmptyMVar, putMVar, tryTakeMVar)
import Control.Lens (view)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text(), unpack)
import System.Exit (ExitCode(..), die)
import System.FilePath (FilePath(), (</>), addTrailingPathSeparator, isPathSeparator, splitDirectories)
import System.Linux.Btrfs (snapshot)
import System.Process (StdStream(CreatePipe), CreateProcess(..), createProcess, getProcessExitCode, proc)

import Btrbkp.Logging
import Btrbkp.Types

createDestinationBuilder :: MonadIO m => Text -> FilePath -> BtrbkpT m FilePath
createDestinationBuilder n s =
    do timestamp <- view timestamp
       sep <- view separator
       bRoot <- view backupRoot
       return (bRoot </> timestamp ++ sep ++ unpack n ++ sep ++ normalize s)

normalize :: FilePath -> FilePath    
normalize d = (foldr1 (\x y -> x ++ '-':y) . filter ((/= 0) . length) . map (filter (not . isPathSeparator)) . splitDirectories) d

syncDirBuilder :: MonadIO m => Text -> FilePath -> BtrbkpT m FilePath
syncDirBuilder n s = do sep <- view separator
                        bRoot <- view backupRoot
                        (return) 
                          (bRoot </> "sync" ++ sep ++ unpack n ++ sep ++ normalize s)

rsync :: MonadIO m => FilePath -> FilePath -> Btrbkp m
rsync src dest = do bkpLog Info (msgTo src' dest)
                    (Just stdIn, Just stdOut, Just stdErr, procHandle) <- (liftIO . createProcess) rsyncProc
                    liftIO (hClose stdIn)
                    mVar <- liftIO newEmptyMVar
                    errThread <- (liftIO . forkIO) (handleToLog stdErr Error mVar)
                    outThread <- (liftIO . forkIO) (handleToLog stdOut Warn mVar)
                    code <- loopLogProcess mVar procHandle
                    case code of
                      ExitSuccess -> bkpLog Info (msg "rsync Succeeded")
                      ExitFailure n -> let m = "rsync exited with code: " ++ show n
                                       in do bkpLog Error (msg m)
                                             liftIO (die m)
   where
     src' = addTrailingPathSeparator src
     rsyncProc = (proc "rsync" ["-avHx", "--delete", src', dest]) { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
     loopLogProcess mVar procHandle = do mVarToLog mVar
                                         result <- liftIO (getProcessExitCode procHandle)
                                         case result of
                                           Nothing -> loopLogProcess mVar procHandle
                                           Just c  -> do mVarToLog mVar
                                                         return c
     mVarToLog mVar = do l <- liftIO $ tryTakeMVar mVar
                         case l of
                           Nothing -> return ()
                           Just (lvl,line)  -> do bkpLog lvl (msg line)
                                                  mVarToLog mVar

handleToLog :: MonadIO m => Handle -> Level -> MVar (Level, String) -> m ()
handleToLog hdl lvl var = do l <- liftIO (hGetLine hdl)
                             liftIO (putMVar var (lvl, l))
                             eof <- liftIO (hIsEOF hdl)
                             if eof
                                then return ()
                                else handleToLog hdl lvl var

backupSnapshot :: MonadIO m => Text -> String -> FilePath -> Btrbkp m
backupSnapshot name src ident = do dest <- createDestinationBuilder name ident
                                   bkpLog Info (src `msgTo` dest)
                                   liftIO (snapshot src dest True)
                                   bkpLog Info (msg "Done")
