{-# LANGUAGE OverloadedStrings #-}
module Btrbkp.Config where

import qualified Data.Attoparsec.Text as P
import Data.Ini (Ini(), parseValue)
import Data.Text (Text())
import System.FilePath (FilePath())

data BtrbkpConfig = BtrbkpConfig { destinations :: [BtrbkpDestination]
                                 , sources      :: [BtrbkpSource]
                                 }

newtype BtrbkpDestination = BtrbkpDestination Text
newtype BtrbkpSource = BtrbkpSource Text

configFromIni :: Ini -> Either String BtrbkpConfig
configFromIni ini = BtrbkpConfig <$> (createDestinations ini) <*> (createSources ini)

createDestinations :: Ini -> Either String [BtrbkpDestination]
createDestinations ini = map BtrbkpDestination <$> parseValue "btrbkp" "destinations" listP ini

createSources :: Ini -> Either String [BtrbkpSource]
createSources ini = map BtrbkpSource <$> parseValue "btrbkp" "sources" listP ini

listP :: P.Parser [Text]
listP = P.manyTill (P.takeWhile (/= listSep) <* P.skipWhile (== listSep)) P.endOfInput
  where
    listSep = ','
