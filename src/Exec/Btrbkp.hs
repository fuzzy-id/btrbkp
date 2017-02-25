module Exec.Btrbkp (main) where

import Control.Applicative (many)
import Data.Ini (readIniFile)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Options.Applicative
        ( Parser(), ParserInfo(), fullDesc, execParser, help, helper, info
        , long, progDesc, short, strOption, value
        )
import System.Exit (ExitCode(ExitSuccess), die, exitWith)

import Btrbkp.Config
import Btrbkp.FrontEnd
import Btrbkp.Types

data Opts = Opts { configFile   :: FilePath
                 , destinations :: [FilePath]
                 , sources      :: [FilePath]
                 }
                 deriving (Eq,Show)

main :: IO ()
main = do opts <- execParser optsDesc
          cfg <- errorOnLeft <$> (readIniFile . configFile) opts
          env <- createEnv cfg
          plan <- planAllBkps cfg
          excs <- catMaybes <$> mapM (runBtrbkp env) plan
          if (length excs == 0)
             then exitWith ExitSuccess
             else (die . show) excs

optsDesc :: ParserInfo Opts
optsDesc = info (helper <*> optsParser) (fullDesc <> progDesc "Backup btrfs volumes.")    

optsParser :: Parser Opts
optsParser = Opts <$> strOption (short 'c' 
                                 <> long "config"
                                 <> help "Configuration file"
                                 <> value "/etc/btrbkp.conf")
                  <*> (many . strOption) (short 'd'
                                          <> long "destination"
                                          <> help "Only backup to these destinations")
                  <*> (many . strOption) (short 's'
                                          <> long "source"
                                          <> help "Only backup these sources")    

errorOnLeft :: Either String b -> b
errorOnLeft = either error id
