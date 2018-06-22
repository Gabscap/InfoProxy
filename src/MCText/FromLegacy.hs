{-# LANGUAGE FlexibleContexts #-}

module MCText.FromLegacy ( fromLegacyText ) where

import qualified Data.Text as T

import Control.Monad.Reader
import Control.Monad.State.Strict

import MCText.BaseComponent
import MCText.ChatColor

fromLegacyText :: T.Text -> [BaseComponent]
fromLegacyText input = runReader (evalStateT fromLegacyText' (T.empty, input)) defaultLC

fromLegacyText' :: StateT (T.Text, T.Text) (Reader LegacyContext) [BaseComponent]
fromLegacyText' = do
    (buffer, rest) <- get
    case rest of
        (T.uncons -> Nothing) -> pure <$> toTextComponent buffer

        (fmap (fmap T.uncons) . T.uncons -> Just ('§', Just (cc, rest'))) ->
            case fromColorCode cc of
                Nothing -> put (buffer `T.snoc` '§' `T.snoc` cc, rest') >> fromLegacyText'
                Just chatColor -> do
                    put (T.empty, rest')
                    if T.null buffer then
                        local (modifyLC chatColor) fromLegacyText'
                    else
                        liftM2 (:) (toTextComponent buffer) (local (modifyLC chatColor) fromLegacyText')

        (T.uncons -> Just (h, rest')) -> do
            put (buffer `T.snoc` h, rest')
            fromLegacyText'

        _ -> error "fromLegacyText': rest case"

    where toTextComponent text = lcToTC text <$> ask

          modifyLC Bold          lc = lc{ lcbold          = True  }
          modifyLC Italic        lc = lc{ lcitalic        = True  }
          modifyLC Underline     lc = lc{ lcunderlined    = True  }
          modifyLC Strikethrough lc = lc{ lcstrikethrough = True  }
          modifyLC Obfuscated    lc = lc{ lcobfuscated    = True  }

          modifyLC Reset         lc = modifyLC White lc
          modifyLC color         _  = defaultLC{ lccolor  = color }

lcToTC :: T.Text -> LegacyContext -> BaseComponent
lcToTC text LC{..} = TextComponent text
                                   (Just lccolor)
                                   (f lcbold)
                                   (f lcitalic)
                                   (f lcunderlined)
                                   (f lcstrikethrough)
                                   (f lcobfuscated)
                                   Nothing
                                   []
    where f True  = Just True
          f False = Nothing

defaultLC :: LegacyContext
defaultLC = LC White False False False False False

data LegacyContext = LC { lccolor         :: ChatColor
                        , lcbold          :: Bool
                        , lcitalic        :: Bool
                        , lcunderlined    :: Bool
                        , lcstrikethrough :: Bool
                        , lcobfuscated    :: Bool }
