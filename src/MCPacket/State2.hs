module MCPacket.State2 ( S2Packet(..)
                       , C2Packet(..)
                       ) where

import qualified Data.Text as T

import Control.Exception
import Data.Int
import Data.Serialize (Putter)

import GetIS
import MCPacket.Internal


data S2Packet = -- Status
    SLoginStart T.Text
    deriving (Show)

data C2Packet = -- Status
    CDisconnect T.Text
    deriving (Show)


instance Packet S2Packet where
    get = getS =<< getVarInt
    put = putS

instance Packet C2Packet where
    get = getC =<< getVarInt
    put = putC


getS :: Int32 -> GetIS S2Packet
getS 0 = SLoginStart <$> getString
getS x = throw.PacketException $ "S2: unkown packet id " ++ show x

putS :: Putter S2Packet
putS (SLoginStart t) = putVarInt 0 >> putString t


getC :: Int32 -> GetIS C2Packet
getC 0 = CDisconnect <$> getString
getC x = throw.PacketException $ "C2: unkown packet id " ++ show x

putC :: Putter C2Packet
putC (CDisconnect t) = putVarInt 0 >> putString t
