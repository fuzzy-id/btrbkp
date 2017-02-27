{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Btrbkp.Types ( BtrbkpEnv(..)
                    , Btrbkp()
                    , BtrbkpT()
                    , FrontEndModule(..)
                    , backupRoot
                    , logger
                    , timestamp
                    , separator
                    , runBtrbkp
                    , runBtrbkpT
                    ) where

import Control.Lens ((^.), makeLenses)
import Control.Monad.Except (ExceptT(), runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT(), runReaderT)
import Data.Ini (Ini())
import Data.Text (Text())
import System.FilePath (FilePath())
import System.Logger (Logger(), flush)

data BkpException = SomeException
                  deriving (Eq,Show)

type BtrbkpT m r = ExceptT BkpException (ReaderT BtrbkpEnv m) r
type Btrbkp m = BtrbkpT m ()

data FrontEndModule c = FrontEndModule { name      :: Text
                                       , configure :: Ini -> Text -> Either String c
                                       , planer    :: forall m. MonadIO m => c -> m [Btrbkp m]
                                       }

data BtrbkpEnv = BtrbkpEnv { _logger     :: Logger
                           , _backupRoot :: FilePath
                           , _timestamp  :: String
                           , _separator  :: String
                           }

makeLenses ''BtrbkpEnv

runBtrbkpT :: BtrbkpT m r -> BtrbkpEnv -> m (Either BkpException r)
runBtrbkpT = runReaderT . runExceptT

runBtrbkp :: MonadIO m => BtrbkpEnv -> Btrbkp m -> m (Maybe BkpException)
runBtrbkp env bkp = do res <- (runBtrbkpT) bkp env
                       flush (env ^. logger)
                       case res of
                         Right () -> return Nothing
                         Left e   -> (return . Just) e
