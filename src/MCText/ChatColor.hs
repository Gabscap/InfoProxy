module MCText.ChatColor ( ChatColor(..)
                        , colorChar
                        , chatColorCode
                        , fromColorCode
                        , chatColorName
                        , fromChatColorName
                        ) where

import qualified Data.Text as T
import           Data.Aeson
import           Data.Char (ord)
import           Data.Maybe (fromMaybe)
import           Data.List (elemIndex)
import           Numeric   (showHex)
import           Control.Exception

-- ChatColor

data ChatColor =
    Black
  | DarkBlue
  | DarkGreen
  | DarkAqua
  | DarkRed
  | DarkPurple
  | Gold
  | Gray
  | DarkGray
  | Blue
  | Green
  | Aqua
  | Red
  | LightPurple
  | Yellow
  | White
  -- Special
  | Obfuscated
  | Bold
  | Strikethrough
  | Underline
  | Italic
  | Reset
    deriving (Eq, Enum, Bounded)

instance Show ChatColor where
    show c = [colorChar, chatColorCode c]

instance FromJSON ChatColor where
    parseJSON = withText "ChatColor" $ \t' -> return $
        let t = T.unpack t'
        in  fromMaybe (throw.MCTextException $ "Unknown ChatColor name: " ++ t) . fromChatColorName $ t

instance ToJSON ChatColor where
    toJSON = String . T.pack . chatColorName

colorChar :: Char
colorChar = '§'

chatColorCode :: ChatColor -> Char
chatColorCode Obfuscated    = 'k'
chatColorCode Bold          = 'l'
chatColorCode Strikethrough = 'm'
chatColorCode Underline     = 'n'
chatColorCode Italic        = 'o'
chatColorCode Reset         = 'r'
chatColorCode c             = head . flip showHex "" . fromEnum $ c

fromColorCode :: Char -> Maybe ChatColor
fromColorCode 'k' = Just Obfuscated
fromColorCode 'l' = Just Bold
fromColorCode 'm' = Just Strikethrough
fromColorCode 'n' = Just Underline
fromColorCode 'o' = Just Italic
fromColorCode 'r' = Just Reset
fromColorCode c
    | ord c >= 48 && ord c <= 57  = Just . toEnum $ ord c - 48
    | ord c >= 65 && ord c <= 70  = Just . toEnum $ ord c - 55
    | ord c >= 97 && ord c <= 102 = Just . toEnum $ ord c - 87
    | otherwise                   = Nothing

chatColorNames :: [String]
chatColorNames =
    [ "black"
    , "dark_blue"
    , "dark_green"
    , "dark_aqua"
    , "dark_red"
    , "dark_purple"
    , "gold"
    , "gray"
    , "dark_gray"
    , "blue"
    , "green"
    , "aqua"
    , "red"
    , "light_purple"
    , "yellow"
    , "white"
    , "obfuscated"
    , "bold"
    , "strikethrough"
    , "underline"
    , "italic"
    , "reset"
    ]

chatColorName :: ChatColor -> String
chatColorName c = chatColorNames !! fromEnum c

fromChatColorName :: String -> Maybe ChatColor
fromChatColorName = fmap toEnum . flip elemIndex chatColorNames

-- Stuff

newtype MCTextException = MCTextException String
instance Show MCTextException where
    show (MCTextException msg) = "MCTextException: " ++ msg
instance Exception MCTextException
