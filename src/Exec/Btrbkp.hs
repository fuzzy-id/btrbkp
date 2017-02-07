module Exec.Btrbkp (main) where

import Data.Ini (readIniFile)
import Data.Maybe (maybe)
import Options.Applicative
import System.Exit (ExitCode(ExitSuccess), die, exitWith)

import Btrbkp
import Btrbkp.Config

data Opts = Opts { configFile :: FilePath
                 }
                 deriving (Eq,Show)

main :: IO ()
main = do opts <- execParser optsDesc
          ini <- errorOnLeft <$> (readIniFile . configFile) opts
          let cfg = errorOnLeft $ configFromIni ini
          maybeExc <- runBkp cfg
          maybe (exitWith ExitSuccess) (die . show) maybeExc
  where
    optsDesc = info (helper <*> optsParser) (fullDesc <> progDesc "Backup btrfs volumes.")
    optsParser = Opts <$> strOption (short 'c' 
                                     <> long "config"
                                     <> help "Configuration file"
                                     <> value "/etc/btrbkp.conf")

errorOnLeft :: Either String b -> b
errorOnLeft = either error id
