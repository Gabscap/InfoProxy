module MCText.BaseComponent ( BaseComponent(..)
                            , singleBaseComponent
                            ) where

import qualified Data.Text as T
import           Data.Aeson
import           Data.Maybe (fromMaybe, catMaybes)

import           MCText.ChatColor

data BaseComponent =
    TextComponent { text          :: T.Text
                  , color         :: Maybe ChatColor
                  , bold          :: Maybe Bool
                  , italic        :: Maybe Bool
                  , underlined    :: Maybe Bool
                  , strikethrough :: Maybe Bool
                  , obfuscated    :: Maybe Bool
                  , insertion     :: Maybe T.Text
                  , extra         :: [BaseComponent] }
    deriving (Show)

instance FromJSON BaseComponent where
    parseJSON = withObject "BaseComponent" $ \o -> do
        text  <- o .: "text"
        color <- o .:? "color"

        bold          <- o .:? "bold"
        italic        <- o .:? "italic"
        underlined    <- o .:? "underlined"
        strikethrough <- o .:? "strikethrough"
        obfuscated    <- o .:? "obfuscated"

        insertion <- o .:? "insertion"
        extra     <- fromMaybe [] <$> o .:? "extra"

        return TextComponent{..}

instance ToJSON BaseComponent where
    toJSON TextComponent{..} = object $ catMaybes [
          "text"          .=? Just text
        , "color"         .=? color
        , "bold"          .=? bold
        , "italic"        .=? italic
        , "underlined"    .=? underlined
        , "strikethrough" .=? strikethrough
        , "obfuscated"    .=? obfuscated
        , "insertion"     .=? insertion
        , "extra" .=? if null extra then Nothing else Just extra
        ]

infixr 8 .=?
(.=?) :: (ToJSON v, KeyValue kv) => T.Text -> Maybe v -> Maybe kv
k .=? v = (k .=) <$> v

singleBaseComponent :: [BaseComponent] -> BaseComponent
singleBaseComponent = TextComponent "" Nothing Nothing Nothing Nothing Nothing Nothing Nothing
