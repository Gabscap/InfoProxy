module MCPacket.State0 ( S0Packet(..) ) where

import qualified Data.Text as T

import Control.Exception
import Data.Int
import Data.Serialize (Putter)

import GetIS
import MCPacket.Internal


data S0Packet =
    SHandshake { hProto :: Int32
               , hHost  :: T.Text
               , hPort  :: Int
               , hNext  :: Int32 }
    deriving (Show)


instance Packet S0Packet where
    get = getS =<< getVarInt
    put = putS


getS :: Int32 -> GetIS S0Packet

getS 0 = do
    hProto <- getVarInt
    hHost  <- getString
    hPort  <- getShort
    hNext  <- getVarInt
    return SHandshake{..}

getS x = throw.PacketException $ "unkown packet id " ++ show x


putS :: Putter S0Packet
putS SHandshake{..} = do
    putVarInt 0
    putVarInt hProto
    putString hHost
    putShort hPort
    putVarInt hNext
