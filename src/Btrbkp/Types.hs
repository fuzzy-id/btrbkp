{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Btrbkp.Types ( BtrbkpEnv(..)
                    , BtrbkpT()
                    , FrontEndModule(..)
                    , backupRoot
                    , logger
                    , destinationBuilder
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

type BtrbkpT m = ExceptT BkpException (ReaderT BtrbkpEnv m) ()

data FrontEndModule c = FrontEndModule { name      :: Text
                                       , configure :: Ini -> Text -> Either String c
                                       , planer    :: forall m. MonadIO m => c -> m [BtrbkpT m]
                                       }

data BtrbkpEnv = BtrbkpEnv { _logger             :: Logger
                           , _destinationBuilder :: Text -> FilePath -> FilePath
                           , _backupRoot         :: FilePath
                           }

makeLenses ''BtrbkpEnv

runBtrbkpT :: MonadIO m => BtrbkpEnv -> BtrbkpT m -> m (Maybe BkpException)
runBtrbkpT env bkp = do res <- (runReaderT . runExceptT) bkp env
                        flush (env ^. logger)
                        case res of
                          Right () -> return Nothing
                          Left e   -> (return . Just) e
