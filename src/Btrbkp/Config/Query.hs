module Btrbkp.Config.Query where

import Prelude hiding (takeWhile)

import Data.Attoparsec.Text (Parser(), endOfInput, manyTill, skipWhile, takeWhile)
import Data.Ini (Ini(), lookupValue, parseValue, sections)
import Data.Text (Text(), pack, unpack)

queryList :: Ini -> Text -> Text -> Either String [Text]
queryList ini sect key = parseValue sect key listP ini

listP :: Parser [Text]
listP =
    manyTill (takeWhile (/= listSep) <* skipWhile (== listSep)) endOfInput
  where
    listSep = ','
