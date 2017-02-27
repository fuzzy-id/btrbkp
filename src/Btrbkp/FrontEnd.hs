module Btrbkp.FrontEnd (planAllBkps) where

import Control.Monad.IO.Class (MonadIO)
import Data.Ini (Ini(), sections)
import Data.Text (isPrefixOf)

import Btrbkp.FrontEnd.LocalSyncs (localSyncsFrontend)
import Btrbkp.FrontEnd.Snapshots (snapshotsFrontEnd)
import Btrbkp.Types (Btrbkp(), FrontEndModule(..))


planAllBkps :: MonadIO m => Ini -> m [Btrbkp m]
planAllBkps cfg = concat <$> mapM ($ cfg) [ configureFrontEnd snapshotsFrontEnd
                                          , configureFrontEnd localSyncsFrontend
                                          ]

configureFrontEnd :: MonadIO m => FrontEndModule c -> Ini -> m [Btrbkp m]
configureFrontEnd mod cfg = concat <$> mapM (planer mod) configs
  where
    relevantSects = (filter (name mod `isPrefixOf`) . sections) cfg
    configs = map (either error id . (configure mod) cfg) relevantSects
