{-# LANGUAGE DeriveGeneric #-}

module Config where

import qualified Data.ByteString as BS
import qualified Data.Text as T

import Data.Yaml
import GHC.Generics


data Config = Config { servers :: [Server] }
    deriving (Show, Generic)

data Server = Server { address     :: Address
                     , kickMessage :: T.Text
                     , motd        :: T.Text
                     , maxplayers  :: Int
                     , online      :: Int
                     , serverbrand :: T.Text
                     , protocol    :: Int
                     , serverIcon  :: FilePath }
    deriving (Show, Generic)

data Address = Address { host :: String
                       , port :: Int }
instance Show Address where
    show Address{..} = host ++ ":" ++ show port


instance FromJSON Config
instance FromJSON Server where
    parseJSON = withObject "Server" $ \o -> do
        address     <- o .: "address"
        kickMessage <- o .: "kickMessage"
        motd        <- o .: "motd"
        maxplayers  <- o .: "maxplayers"
        online      <- o .:? "online" .!= 0
        serverbrand <- o .:? "serverbrand" .!= "InfoProxy"
        protocol    <- o .: "protocol"
        serverIcon  <- o .: "serverIcon"
        return Server{..}


instance FromJSON Address where
    parseJSON = withText "Address" $ \t ->
        let [host,port'] = T.unpack <$> T.splitOn ":" t
            port = read port'
        in return Address{..}

instance ToJSON Config
instance ToJSON Server where
    -- only for initial config
    toJSON Server{..} = object [
        "address"     .= address
      , "kickMessage" .= kickMessage
      , "motd"        .= motd
      , "maxplayers"  .= maxplayers
      , "protocol"    .= protocol
      , "serverIcon"  .= serverIcon ]

instance ToJSON Address where
    toJSON = String . T.pack . show


parseConfigFile :: BS.ByteString -> Either String Config
parseConfigFile = decodeEither

sampleConfig :: Config
sampleConfig = Config [ Server (Address "127.0.0.1" 25566)
                               "§cDefault kick message"
                               "§cDefault motd"
                               100
                               0
                               "InfoProxy"
                               340
                               "server-icon.png" ]

sampleConfigText :: BS.ByteString
sampleConfigText = encode sampleConfig
