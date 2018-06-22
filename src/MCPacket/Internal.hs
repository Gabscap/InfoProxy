module MCPacket.Internal where

import Prelude

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Bytes.Serial
import Data.Bytes.VarInt
import Data.Int
import Data.Serialize as S

import Control.Exception
import Control.Monad

import GetIS


class Show p => Packet p where
    get :: GetIS p
    put :: Putter p

-- Helper

getString :: GetIS T.Text
getString = do
    l <- getVarInt
    when (l > 32678) $ throw.PacketException $ "String length > 32678"
    T.decodeUtf8 <$> getIS (getByteString $ fromIntegral l)

getVarInt :: GetIS Int32
getVarInt = unVarInt <$> (limit 5 . getIS) (deserialize :: Get (VarInt Int32))

getShort :: Num a => GetIS a
getShort = fromIntegral <$> getIS getWord16be

getLong :: Num a => GetIS a
getLong = fromIntegral <$> getIS getWord64be

-- Put

putVarInt :: Putter Int32
putVarInt = serialize . VarInt

putString :: Putter T.Text
putString t = do
    let bs = T.encodeUtf8 t
    putVarInt.fromIntegral $ BS.length bs
    putByteString bs

putShort :: Integral a => Putter a
putShort = putWord16be . fromIntegral

putLong :: Putter Int64
putLong = putInt64be

newtype PacketException = PacketException String
instance Show PacketException where
    show (PacketException msg) = "PacketException: " ++ msg
instance Exception PacketException
