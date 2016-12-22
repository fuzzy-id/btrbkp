module Exec.Btrbkp (main) where

import Options.Applicative

data Opts = Opts { configFile :: FilePath
                 }
                 deriving (Eq,Show)


main :: IO ()
main = do opts <- execParser optsDesc
          undefined
  where
    optsDesc = info (helper <*> optsParser) (fullDesc <> progDesc "Backup btrfs volumes.")
    optsParser = Opts <$> strOption (short 'c' 
                                     <> long "config"
                                     <> help "Configuration file"
                                     <> value "/etc/btrbkp.conf")
