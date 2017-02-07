{-# LANGUAGE TemplateHaskell #-}
module Btrbkp where

import Control.Lens ((^.), makeLenses, view)
import Control.Monad.Except (ExceptT(), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT(), runReaderT)
import Data.Hourglass (DateTime(), timePrint)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Data.Traversable (forM)
import System.Linux.Btrfs (snapshot)
import System.FilePath ((</>), pathSeparator, splitDirectories)
import qualified System.Logger as L
import Time.System (dateCurrent)

import Btrbkp.Config
import Util.Findmnt (findmnt)

data BtrbkpEnv = BtrbkpEnv { _getLogger :: L.Logger
                           , _getDateTime :: DateTime
                           }

makeLenses ''BtrbkpEnv

data BkpException = SomeException
                  deriving (Eq,Show)

type BtrbkpT m = ExceptT BkpException (ReaderT BtrbkpEnv m) ()

runBtrbkp :: MonadIO m => BtrbkpEnv -> BtrbkpT m -> m (Maybe BkpException)
runBtrbkp env bkp = do res <- (runReaderT . runExceptT) bkp env
                       case res of
                         Right () -> return Nothing
                         Left e   -> (return . Just) e

btrbkp :: MonadIO m => FilePath -> FilePath -> BtrbkpT m
btrbkp src dest = do timeStamp <- timePrint "YYYY-MM-DD_H-MI-S" <$> view getDateTime
                     let dest' = dest </> timeStamp ++ ":" ++ (last . splitDirectories) src
                     bkpLog L.Info (src `msgTo` dest')
                     liftIO (snapshot src dest' True)
                     bkpLog L.Info (L.msg "Done")

msgTo :: String -> String -> L.Msg -> L.Msg
msgTo s d = L.msg (s ++ " -> " ++ d)

bkpLog :: MonadIO m => L.Level -> (L.Msg -> L.Msg) -> BtrbkpT m
bkpLog lvl msg = do l <- view getLogger
                    liftIO (L.log l lvl msg)

runBkp :: MonadIO m => BtrbkpConfig -> m (Maybe BkpException)
runBkp cfg = do env <- createEnv cfg
                bMap <- mkBkpMap (cfg ^. destinations) (cfg ^. sources)
                unfoldBkp env bMap
  where
    unfoldBkp :: MonadIO m => BtrbkpEnv -> [BtrbkpT m] -> m (Maybe BkpException)
    unfoldBkp env []     = do finalize env
                              return Nothing
    unfoldBkp env (b:bs) = do res <- runBtrbkp env b
                              if isNothing res
                                 then unfoldBkp env bs
                                 else do finalize env
                                         return res
    finalize env = do L.flush (env ^. getLogger)
                      L.close (env ^. getLogger)

createEnv :: MonadIO m => BtrbkpConfig -> m BtrbkpEnv
createEnv cfg = BtrbkpEnv <$> createLogger cfg
                          <*> liftIO dateCurrent

createLogger :: MonadIO m => BtrbkpConfig -> m L.Logger
createLogger _ = (liftIO . L.create) L.StdOut

mkBkpMap :: MonadIO m => [BtrbkpDestination] -> [BtrbkpSource] -> m [BtrbkpT m]
mkBkpMap []        _        = return []
mkBkpMap _         []       = return []
mkBkpMap (d:dests) (s:srcs) = do bkp <- analyze d s
                                 thisDest <- (bkp:) <$> mkBkpMap (d:dests) srcs
                                 (thisDest `mappend`) <$> mkBkpMap dests (s:srcs)

analyze :: MonadIO m => BtrbkpDestination -> BtrbkpSource -> m (BtrbkpT m)
analyze (BtrbkpDestination d) (BtrbkpSource s) = do srcUuid <- findmnt src
                                                    tgtUuid <- findmnt dest
                                                    if srcUuid == tgtUuid
                                                       then return $ btrbkp src dest
                                                       else undefined
                                                    
  where
    (src, dest) = (T.unpack s, T.unpack d)
