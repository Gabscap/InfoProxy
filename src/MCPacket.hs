module MCPacket ( module MCPacket.State0
                , module MCPacket.State1
                , module MCPacket.State2
                , readPacket
                , writePacket
                ) where


import qualified Data.ByteString as BS
import           System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Cereal as Streams

import Data.Serialize hiding (get)

import Control.Exception
import Control.Monad

import GetIS

import MCPacket.State0
import MCPacket.State1
import MCPacket.State2
import MCPacket.Internal as MCP

maxPacketLength = 1024*1024


readPacket :: Packet p => InputStream BS.ByteString -> IO p
readPacket stream = runGetIS decodePacket stream

writePacket :: Packet p => OutputStream BS.ByteString -> p -> IO ()
writePacket os p = do
    bs <- Streams.putToStream $ encodePacket p
    Streams.supply bs os

decodePacket :: Packet p => GetIS p
decodePacket = do
    size <- getVarInt
    when (size < 0) $ throw.PacketException $ "Packet size < 0"
    when (size > maxPacketLength) $ throw.PacketException $ "Packet size > 1KB"

    (packet, c) <- count $ limit (fromIntegral size) get

    when (c /= fromIntegral size) $ throw.PacketException $ "Declared packet length (" ++ show size ++ ") > real packet length (" ++ show c ++ ")"
    return packet

encodePacket :: Packet p => Putter p
encodePacket p = do
    let res = runPut $ MCP.put p
    putVarInt.fromIntegral $ BS.length res
    putByteString res
