module Btrbkp.Config.Query where

import Prelude hiding (takeWhile)

import Data.Attoparsec.Text (Parser(), endOfInput, manyTill, skipWhile, takeWhile)
import Data.Ini (Ini(), keys, lookupValue, parseValue, readValue, sections)
import Data.Text (Text(), pack, unpack)
import Data.Text.Read (decimal)

queryIntWithDefault :: Int -> Ini -> Text -> Text -> Either String Int
queryIntWithDefault def cfg sect key = (readValue sect key decimal cfg)

querySingleWithDefault :: Text -> Ini -> Text -> Text -> Either String Text
querySingleWithDefault def ini sect key =
    if Right True == fmap (elem key) (keys sect ini)
       then lookupValue sect key ini
       else Right def

queryList :: Ini -> Text -> Text -> Either String [Text]
queryList ini sect key = parseValue sect key listP ini

listP :: Parser [Text]
listP =
    manyTill (takeWhile (/= listSep) <* skipWhile (== listSep)) endOfInput
  where
    listSep = ','
