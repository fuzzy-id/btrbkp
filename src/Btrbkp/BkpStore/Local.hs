module Btrbkp.BkpStore.Local ( backupSnapshot
                             , createDestinationBuilder
                             , normalize
                             , rsyncLocal
                             , rsyncRemote
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
import System.Process (CmdSpec(..), CreateProcess(..), StdStream(CreatePipe), createProcess, getProcessExitCode, proc)

import Btrbkp.Logging
import Btrbkp.Types

createDestinationBuilder :: MonadIO m => Text -> FilePath -> BtrbkpT m FilePath
createDestinationBuilder n s =
    do timestamp <- view timestamp
       sep <- view separator
       bRoot <- view backupRoot
       return (bRoot </> timestamp ++ sep ++ unpack n ++ sep ++ normalize s)

normalize :: FilePath -> FilePath    
normalize d = (foldr1 (\x y -> x ++ '-':y) 
               . filter ((/= 0) . length) 
               . map (filter (not . isPathSeparator)) 
               . splitDirectories
               ) d

syncDirBuilder :: MonadIO m => Text -> FilePath -> BtrbkpT m FilePath
syncDirBuilder n s = do sep <- view separator
                        bRoot <- view backupRoot
                        (return) 
                          (bRoot </> "sync" ++ sep ++ unpack n ++ sep ++ normalize s)

rsyncLocal :: MonadIO m => FilePath -> FilePath -> Btrbkp m
rsyncLocal src dest = do bkpLog Info (msgTo src' dest)
                         callRsync [src', dest]
   where
     src' = addTrailingPathSeparator src
     

rsyncRemote :: MonadIO m => String -> Int -> FilePath -> FilePath -> [String] -> Btrbkp m
rsyncRemote host port path dest exArgs = do bkpLog Info (msgTo remotePath dest)
                                            callRsync args
  where
    remotePath = host ++ '@' : addTrailingPathSeparator path
    args = exArgs ++ [remotePath, dest]

callRsync :: MonadIO m => [String] -> Btrbkp m
callRsync extraArgs = do code <- logProcess rsyncProc
                         case code of
                           ExitSuccess -> bkpLog Info (msg "rsync Succeeded")
                           ExitFailure n -> let m = "rsync exited with code: " ++ show n
                                            in do bkpLog Error (msg m)
                                                  liftIO (die m)
  where
    rsyncProc = proc "rsync" (defaultArgs ++ extraArgs)
    defaultArgs = ["-avHx", "--delete", "--delete-excluded", "--numeric-ids"]

logProcess :: MonadIO m => CreateProcess -> BtrbkpT m ExitCode
logProcess p = do (bkpLog Info . msg . ("Calling: " ++) . getCmd) p
                  (Just stdIn, Just stdOut, Just stdErr, procHandle) <- (liftIO . createProcess) p'
                  liftIO (hClose stdIn)
                  mVar <- liftIO newEmptyMVar
                  errThread <- (liftIO . forkIO) (handleToLog stdErr Error mVar)
                  outThread <- (liftIO . forkIO) (handleToLog stdOut Warn mVar)
                  loopLogProcess mVar procHandle
  where
    p' = p { std_in = CreatePipe, std_out = CreatePipe , std_err = CreatePipe }
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

getCmd p = cmd
  where
    cmd = toString $ cmdspec p
    toString (ShellCommand s) = s
    toString (RawCommand c args) = c ++ ' ' : show args
