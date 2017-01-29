{-# LANGUAGE TemplateHaskell #-}
module Btrbkp where

import qualified Data.Text as T
import Control.Lens ((^.), makeLenses, view)
import Control.Monad.Except (ExceptT(), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT(), runReaderT)
import System.Linux.Btrfs (snapshot)
import qualified System.Logger as L
import Time.System (dateCurrent)

import Btrbkp.Config

data BtrbkpEnv = BtrbkpEnv { _getLogger :: L.Logger
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
btrbkp src dest = do bkpLog L.Info (src `msgTo` dest)
                     liftIO (snapshot dest src True)
                     bkpLog L.Info (L.msg "Done")

msgTo :: String -> String -> L.Msg -> L.Msg
msgTo s d = L.msg (s ++ " -> " ++ d)

bkpLog :: MonadIO m => L.Level -> (L.Msg -> L.Msg) -> BtrbkpT m
bkpLog lvl msg = do l <- view getLogger
                    liftIO (L.log l lvl msg)

runBkp :: MonadIO m => BtrbkpConfig -> m ()
runBkp cfg = do env <- createEnv cfg
                bMap <- mkBkpMap (cfg ^. destinations) (cfg ^. sources)
                undefined

createEnv :: MonadIO m => BtrbkpConfig -> m BtrbkpEnv
createEnv cfg = BtrbkpEnv <$> createLogger cfg

createLogger :: MonadIO m => BtrbkpConfig -> m L.Logger
createLogger _ = (liftIO . L.create) L.StdOut

mkBkpMap :: MonadIO m => [BtrbkpDestination] -> [BtrbkpSource] -> m [BtrbkpT m]
mkBkpMap [] _ = return []
mkBkpMap (d:dests) (s:srcs) = do bkp <- analyze d s
                                 thisDest <- (bkp:) <$> mkBkpMap dests (s:srcs)
                                 (thisDest `mappend`) <$> mkBkpMap dests srcs

analyze (BtrbkpDestination d) (BtrbkpSource s) = return $ btrbkp (T.unpack s) (T.unpack d)
