{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Btrbkp.Config where

import Control.Lens (makeLenses)
import qualified Data.Attoparsec.Text as P
import Data.Ini (Ini(), lookupValue, parseValue, sections)
import Data.Text (Text())
import System.FilePath (FilePath())

newtype BtrbkpDestination = BtrbkpDestination Text deriving (Eq,Show)
newtype BtrbkpSource = BtrbkpSource Text deriving (Eq,Show)

data BtrbkpConfig = BtrbkpConfig { _destinations :: [BtrbkpDestination]
                                 , _sources      :: [BtrbkpSource]
                                 }
                                 deriving (Eq,Show)

makeLenses ''BtrbkpConfig

configFromIni :: Ini -> Either String BtrbkpConfig
configFromIni ini = BtrbkpConfig <$> createDestinations ini <*> createSources ini

createDestinations :: Ini -> Either String [BtrbkpDestination]
createDestinations ini = mapM (lookupDestSect ini) =<< parseValue "btrbkp" "destinations" listP ini

createSources :: Ini -> Either String [BtrbkpSource]
createSources ini = mapM (lookupSourceSect ini) =<< parseValue "btrbkp" "sources" listP ini

listP :: P.Parser [Text]
listP = P.manyTill (P.takeWhile (/= listSep) <* P.skipWhile (== listSep)) P.endOfInput
  where
    listSep = ','

lookupDestSect :: Ini -> Text -> Either String BtrbkpDestination
lookupDestSect ini d
  | d `elem` sections ini = BtrbkpDestination <$> lookupValue d "path" ini
  | otherwise               = (Right . BtrbkpDestination) d

lookupSourceSect :: Ini -> Text -> Either String BtrbkpSource
lookupSourceSect ini s
  | s `elem` sections ini = BtrbkpSource <$> lookupValue s "path" ini
  | otherwise               = (Right . BtrbkpSource) s
