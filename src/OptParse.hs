module OptParse where

import Options.Applicative
import Data.Semigroup ((<>))
--
-- CLI parsing
--
data Options = Options
    { oDebug    :: Bool
    , oConfig   :: String
    } deriving (Show)

parseCLI :: IO Options
parseCLI = execParser (withInfo parseOptions "infoproxy")
  where
    withInfo opts h = info (helper <*> opts) $ header h

parseOptions :: Parser Options
parseOptions = Options
    <$> pure False
      --  switch
      --  ( long "debug"
      -- <> short 'd'
      -- <> help "Enable debug output" )
    <*> strOption
        ( long "file"
       <> short 'f'
       <> metavar "FILE"
       <> value "config.yml"
       <> help "Path to config yml file" )
