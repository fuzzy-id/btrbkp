{-# LANGUAGE TemplateHaskell #-}
module Btrbkp where

import Control.Lens (makeLenses, view)
import Control.Monad.Except (ExceptT())
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT())
import System.Linux.Btrfs (snapshot)
import qualified System.Logger as L
import Time.System (dateCurrent)

data BtrfsSnapshot = BtrfsSnapshot { _getDestinationPath :: FilePath
                                   , _getSourcePath      :: FilePath
                                   , _getLogger          :: L.Logger
                                   }

makeLenses ''BtrfsSnapshot

data BkpException = SomeException
                  deriving (Eq,Show)

type BtrbkpT m a = ExceptT BkpException (ReaderT BtrfsSnapshot m) a

btrbkp :: MonadIO m => BtrbkpT m ()
btrbkp = do dest <- view getDestinationPath
            src  <- view getSourcePath
            bkpLog L.Info (src `msgTo` dest)
            liftIO (snapshot dest src True)

msgTo :: String -> String -> L.Msg -> L.Msg
msgTo s d = L.msg (s ++ " -> " ++ d)

bkpLog :: MonadIO m => L.Level -> (L.Msg -> L.Msg) -> BtrbkpT m ()
bkpLog lvl msg = do l <- view getLogger
                    liftIO (L.log l lvl msg)
