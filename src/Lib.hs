module Lib
    ( encode
    , decode
    ) where

import Data.Bits
import Data.Word
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL


parity :: V.Vector Word8
parity = V.fromList
  [ 0x0, 0x3, 0x5, 0x6
  , 0x6, 0x5, 0x3, 0x0
  , 0x7, 0x4, 0x2, 0x1
  , 0x1, 0x2, 0x4, 0x7 ]

transmit :: V.Vector Word8
transmit = V.fromList
  [ 0x00, 0x07, 0x19, 0x1e
  , 0x2a, 0x2d, 0x33, 0x34
  , 0x4b, 0x4c, 0x52, 0x55
  , 0x61, 0x66, 0x78, 0x7f ]

encode :: BL.ByteString -> BL.ByteString
encode = BL.pack . concatMap (flatten . encodeWord) . BL.unpack
  where
    flatten (u, l) = [u, l]

decode :: BL.ByteString -> BL.ByteString
decode = BL.pack . map decodeWord . unflatten . BL.unpack
  where
    unflatten (u:l:rest) = (u, l) : unflatten rest
    unflatten [_] = error "wrong number of bytes in input stream"
    unflatten [] = []

encodeWord :: Word8 -> (Word8, Word8)
encodeWord = encodeHalves . splitWord

decodeWord :: (Word8, Word8) -> Word8
decodeWord (u, l) = let
  gotU = extractParity u
  expU = V.unsafeIndex parity $ fromIntegral (extractData u)
  gotL = extractParity l
  expL = V.unsafeIndex parity $ fromIntegral (extractData l)
  upper = (extractData . correct gotU expU) u
  lower = (extractData . correct gotL expL) l
  
  in unsplitWord (upper, lower)


splitWord :: Word8 -> (Word8, Word8)
splitWord w = (shiftR (w .&. 0xf0) 4, w .&. 0xf)

encodeHalves :: (Word8, Word8) -> (Word8, Word8)
encodeHalves (upper, lower) = (V.unsafeIndex transmit u, V.unsafeIndex transmit l)
  where (u, l) = (fromIntegral upper, fromIntegral lower)

unsplitWord :: (Word8, Word8) -> Word8
unsplitWord (u, l) = shiftL u 4 .|. l

correct :: Word8 -> Word8 -> Word8 -> Word8
correct p1 p2 w = w `xor` (shiftR (shiftL 1 diff) 1 :: Word8)
  where
    diff = fromIntegral (p1 `xor` p2) :: Int

extractParity :: Word8 -> Word8
extractParity w = let
  p2 = shiftR (w .&. 0x08) 1
  p1 = w .&. 0x02
  p0 = w .&. 0x01
  
  in p2 .|. p1 .|. p0

extractData :: Word8 -> Word8
extractData w = let
  d3 = shiftR (w .&. 0x40) 3
  d2 = shiftR (w .&. 0x20) 3
  d1 = shiftR (w .&. 0x10) 3
  d0 = shiftR (w .&. 0x04) 2

  in d3 .|. d2 .|. d1 .|. d0