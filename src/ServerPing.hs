{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module ServerPing where

import qualified Data.Text as T

import           Data.Aeson
import           Data.Maybe (catMaybes)
import           Control.Exception
import           GHC.Generics
import           MCText
import           Control.Monad
import           Control.Applicative ((<|>))

data PingException = PingException String
instance Show PingException where
    show (PingException msg) = "PingException: " ++ msg
instance Exception PingException

data ServerPing =
    ServerPing { description :: Description
               , players     :: Players
               , version     :: Version
               , favicon     :: Maybe T.Text }
    deriving (Generic, Show)

data Description = DString T.Text
                 | DMCText BaseComponent
    deriving (Show)

data Players = Players { max    :: Int
                       , online :: Int
                       , sample :: Maybe [Player] }
    deriving (Generic, Show)

data Player = Player { name :: T.Text
                     , id   :: T.Text }
    deriving (Generic, Show)

data Version = Version { name     :: T.Text
                       , protocol :: Int }
    deriving (Generic, Show)

instance FromJSON Players
instance FromJSON Player
instance FromJSON Version
instance FromJSON Description where
    parseJSON = (DString <$$> withText "Description17" return)
           <||> (DMCText <$$> parseJSON)
        where (<$$>) = (<$>).(<$>)
              (<||>) = liftM2 (<|>)

instance FromJSON ServerPing

instance ToJSON Players where
    toJSON Players{..} = object $ catMaybes [
          "max"    .=? Just max
        , "online" .=? Just online
        , "sample" .=? sample
        ]
instance ToJSON Player
instance ToJSON Version
instance ToJSON Description where
    toJSON (DString t) = String t
    toJSON (DMCText t) = toJSON t
instance ToJSON ServerPing where
    toJSON ServerPing{..} = object $ catMaybes [
          "description" .=? Just description
        , "players"     .=? Just players
        , "version"     .=? Just version
        , "favicon"     .=? favicon
        ]

infixr 8 .=?
(.=?) :: (ToJSON v, KeyValue kv) => T.Text -> Maybe v -> Maybe kv
k .=? v = (k .=) <$> v
