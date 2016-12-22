module Exec.Btrbkp (main) where

import Data.Ini (readIniFile)
import Options.Applicative

import Btrbkp.Config

data Opts = Opts { configFile :: FilePath
                 }
                 deriving (Eq,Show)


main :: IO ()
main = do opts <- execParser optsDesc
          ini <- errorOnLeft <$> (readIniFile . configFile) opts
          let cfg = errorOnLeft $ configFromIni ini
          undefined
  where
    optsDesc = info (helper <*> optsParser) (fullDesc <> progDesc "Backup btrfs volumes.")
    optsParser = Opts <$> strOption (short 'c' 
                                     <> long "config"
                                     <> help "Configuration file"
                                     <> value "/etc/btrbkp.conf")

errorOnLeft :: Either String b -> b
errorOnLeft = either error id
