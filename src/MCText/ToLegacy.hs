module MCText.ToLegacy ( toLegacyText
                       , toLegacyText'
                       ) where

import qualified Data.Text as T
import           Data.Maybe (fromMaybe)
import           Control.Monad.Reader

import           MCText.BaseComponent
import           MCText.ChatColor

-- [BaseComponent] -> Legacy Text
toLegacyText :: [BaseComponent] -> T.Text
toLegacyText = flip runReader defaultLC . toLegacyTextR

toLegacyText' :: BaseComponent -> T.Text
toLegacyText' = flip runReader defaultLC . toLegacyTextR'

toLegacyTextR :: [BaseComponent] -> Reader LegacyContext T.Text
toLegacyTextR = fmap T.concat . mapM toLegacyTextR'

toLegacyTextR' :: BaseComponent -> Reader LegacyContext T.Text
toLegacyTextR' t@TextComponent{} = do
    lcbold          <- is' lcbold          bold
    lcitalic        <- is' lcitalic        italic
    lcunderlined    <- is' lcunderlined    underlined
    lcstrikethrough <- is' lcstrikethrough strikethrough
    lcobfuscated    <- is' lcobfuscated    obfuscated

    let result = T.concat [ tShow $ fromMaybe White (color t)
                          , showIf lcbold          Bold
                          , showIf lcitalic        Italic
                          , showIf lcunderlined    Underline
                          , showIf lcstrikethrough Strikethrough
                          , showIf lcobfuscated    Obfuscated
                          , text t ]
    extraText <- local (const LC{..}) $ toLegacyTextR (extra t)
    return $ result `T.append` extraText
    where is' lc l     = liftM2 fromMaybe (asks lc) (return $ l t)
          showIf b c = if b then tShow c else T.empty

defaultLC :: LegacyContext
defaultLC = LC False False False False False

data LegacyContext = LC { lcbold          :: Bool
                        , lcitalic        :: Bool
                        , lcunderlined    :: Bool
                        , lcstrikethrough :: Bool
                        , lcobfuscated    :: Bool }

tShow :: Show s => s -> T.Text
tShow = T.pack . show
