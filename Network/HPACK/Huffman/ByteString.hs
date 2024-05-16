module Network.HPACK.Huffman.ByteString (
    unpack4bits
) where

import Data.ByteString (unpack)

import Imports

-- $setup
-- >>> import qualified Data.ByteString as BS

-- |
--
-- >>> let bs = BS.pack [0x12,0x34,0xf3,0xab]
-- >>> unpack4bits bs
-- [1,2,3,4,15,3,10,11]
-- >>> unpack4bits $ BS.tail bs
-- [3,4,15,3,10,11]
unpack4bits :: ByteString -> [Word8]
unpack4bits bs = concat [[w `shiftR` 4, w .&. 0xf] | w <- unpack bs]
