module MCPacket.State1 ( S1Packet(..)
                       , C1Packet(..)
                       ) where

import qualified Data.Text as T

import Control.Exception

import Data.Int
import Data.Serialize (Putter)

import GetIS
import MCPacket.Internal


data S1Packet = -- Status
    SRequest
  | SPing Int64
    deriving (Show)

data C1Packet = -- Status
    CResponse T.Text
  | CPong Int64
    deriving (Show)

-- Packet Serialization


instance Packet S1Packet where
    get = getS =<< getVarInt
    put = putS

instance Packet C1Packet where
    get = getC =<< getVarInt
    put = putC


getS :: Int32 -> GetIS S1Packet
getS 0 = return SRequest
getS 1 = SPing <$> getLong
getS x = throw.PacketException $ "S1: unkown packet id " ++ show x

putS :: Putter S1Packet
putS SRequest  = putVarInt 0
putS (SPing i) = putVarInt 1 >> putLong i


getC :: Int32 -> GetIS C1Packet
getC 0 = CResponse <$> getString
getC 1 = CPong <$> getLong
getC x = throw.PacketException $ "C1: unkown packet id " ++ show x

putC :: Putter C1Packet
putC (CResponse t) = putVarInt 0 >> putString t
putC (CPong i)     = putVarInt 1 >> putLong i
