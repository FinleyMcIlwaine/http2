{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Frame.Encode (
    encodeFrame,
    encodeFrameChunks,
    encodeFrameHeader,
    encodeFrameHeaderBuf,
    encodeFramePayload,
    EncodeInfo (..),
    encodeInfo,
) where

import qualified Data.ByteString as BS
import Data.ByteString.Internal (create)
import Foreign.Ptr (Ptr, plusPtr)
import qualified Network.ByteOrder as N
import Network.Control (WindowSize)

import Imports
import Network.HTTP2.Frame.Types

----------------------------------------------------------------

type Builder = [ByteString] -> [ByteString]

-- | Auxiliary information for frame encoding.
data EncodeInfo = EncodeInfo
    { encodeFlags :: FrameFlags
    -- ^ Flags to be set in a frame header
    , encodeStreamId :: StreamId
    -- ^ Stream id to be set in a frame header
    , encodePadding :: Maybe Padding
    -- ^ Padding if any. In the case where this value is set but the priority flag is not set, this value gets preference over the priority flag. So, if this value is set, the priority flag is also set.
    }
    deriving (Show, Read)

----------------------------------------------------------------

-- | A smart builder of 'EncodeInfo'.
--
-- >>> encodeInfo setAck 0
-- EncodeInfo {encodeFlags = 1, encodeStreamId = 0, encodePadding = Nothing}
encodeInfo
    :: (FrameFlags -> FrameFlags)
    -> Int
    -- ^ stream identifier
    -> EncodeInfo
encodeInfo set sid = EncodeInfo (set defaultFlags) sid Nothing

----------------------------------------------------------------

-- | Encoding an HTTP/2 frame to 'ByteString'.
-- This function is not efficient enough for high performace
-- program because of the concatenation of 'ByteString'.
--
-- >>> encodeFrame (encodeInfo id 1) (DataFrame "body")
-- "\NUL\NUL\EOT\NUL\NUL\NUL\NUL\NUL\SOHbody"
encodeFrame :: EncodeInfo -> FramePayload -> IO ByteString
encodeFrame einfo payload = BS.concat <$> encodeFrameChunks einfo payload

-- | Encoding an HTTP/2 frame to ['ByteString'].
--   This is suitable for sendMany.
encodeFrameChunks :: EncodeInfo -> FramePayload -> IO [ByteString]
encodeFrameChunks einfo payload = do
    (header, bss) <- encodeFramePayload einfo payload
    bs <- encodeFrameHeader ftid header
    return $ bs : bss
  where
    ftid = framePayloadToFrameType payload

-- | Encoding an HTTP/2 frame header.
--   The frame header must be completed.
encodeFrameHeader :: FrameType -> FrameHeader -> IO ByteString
encodeFrameHeader ftid fhdr = create frameHeaderLength $ encodeFrameHeaderBuf ftid fhdr

-- | Writing an encoded HTTP/2 frame header to the buffer.
--   The length of the buffer must be larger than or equal to 9 bytes.
encodeFrameHeaderBuf :: FrameType -> FrameHeader -> Ptr Word8 -> IO ()
encodeFrameHeaderBuf ftid FrameHeader{..} ptr = do
    N.poke24 plen ptr 0
    N.poke8 typ ptr 3
    N.poke8 flags ptr 4
    N.poke32 sid ptr 5
  where
    plen = fromIntegral payloadLength
    typ = fromFrameType ftid
    sid = fromIntegral streamId

-- | Encoding an HTTP/2 frame payload.
--   This returns a complete frame header and chunks of payload.
encodeFramePayload :: EncodeInfo -> FramePayload -> IO (FrameHeader, [ByteString])
encodeFramePayload einfo payload = do
    (header, builder) <- buildFramePayload einfo payload
    return (header, builder [])

----------------------------------------------------------------

buildFramePayload :: EncodeInfo -> FramePayload -> IO (FrameHeader, Builder)
buildFramePayload einfo (DataFrame body) =
    return $ buildFramePayloadData einfo body
buildFramePayload einfo (HeadersFrame mpri hdr) =
    buildFramePayloadHeaders einfo mpri hdr
buildFramePayload einfo (PriorityFrame pri) =
    buildFramePayloadPriority einfo pri
buildFramePayload einfo (RSTStreamFrame e) =
    return $ buildFramePayloadRSTStream einfo e
buildFramePayload einfo (SettingsFrame settings) =
    buildFramePayloadSettings einfo settings
buildFramePayload einfo (PushPromiseFrame sid hdr) =
    return $ buildFramePayloadPushPromise einfo sid hdr
buildFramePayload einfo (PingFrame opaque) =
    return $ buildFramePayloadPing einfo opaque
buildFramePayload einfo (GoAwayFrame sid e debug) =
    buildFramePayloadGoAway einfo sid e debug
buildFramePayload einfo (WindowUpdateFrame size) =
    return $ buildFramePayloadWindowUpdate einfo size
buildFramePayload einfo (ContinuationFrame hdr) =
    return $ buildFramePayloadContinuation einfo hdr
buildFramePayload einfo (UnknownFrame _ opaque) =
    return $ buildFramePayloadUnknown einfo opaque

----------------------------------------------------------------

buildPadding
    :: EncodeInfo
    -> Builder
    -> Int
    -- ^ Payload length.
    -> (FrameHeader, Builder)
buildPadding EncodeInfo{encodePadding = Nothing, ..} builder len =
    (header, builder)
  where
    header = FrameHeader len encodeFlags encodeStreamId
buildPadding EncodeInfo{encodePadding = Just padding, ..} btarget targetLength =
    (header, builder)
  where
    header = FrameHeader len newflags encodeStreamId
    builder = (b1 :) . btarget . (padding :)
    b1 = BS.singleton $ fromIntegral paddingLength
    paddingLength = BS.length padding
    len = targetLength + paddingLength + 1
    newflags = setPadded encodeFlags

buildPriority :: Priority -> IO Builder
buildPriority Priority{..} = do
    priority <- create 5 $ \ptr -> do
        let esid = fromIntegral estream
            w = fromIntegral $ weight - 1
        N.poke32 esid ptr 0
        N.poke8 w ptr 4
    return (priority :)
  where
    estream
        | exclusive = setExclusive streamDependency
        | otherwise = streamDependency

----------------------------------------------------------------

buildFramePayloadData :: EncodeInfo -> ByteString -> (FrameHeader, Builder)
buildFramePayloadData einfo body = buildPadding einfo builder len
  where
    builder = (body :)
    len = BS.length body

buildFramePayloadHeaders
    :: EncodeInfo
    -> Maybe Priority
    -> HeaderBlockFragment
    -> IO (FrameHeader, Builder)
buildFramePayloadHeaders einfo Nothing hdr =
    return $ buildPadding einfo builder len
  where
    builder = (hdr :)
    len = BS.length hdr
buildFramePayloadHeaders einfo (Just pri) hdr = do
    builder <-(. (hdr :)) <$> buildPriority pri
    return $ buildPadding einfo' builder len
  where
    len = BS.length hdr + 5
    einfo' = einfo{encodeFlags = setPriority (encodeFlags einfo)}

buildFramePayloadPriority :: EncodeInfo -> Priority -> IO (FrameHeader, Builder)
buildFramePayloadPriority EncodeInfo{..} p = do
    builder <- buildPriority p
    return (header, builder)
  where
    header = FrameHeader 5 encodeFlags encodeStreamId

buildFramePayloadRSTStream :: EncodeInfo -> ErrorCode -> (FrameHeader, Builder)
buildFramePayloadRSTStream EncodeInfo{..} e = (header, builder)
  where
    builder = (b4 :)
    b4 = N.bytestring32 $ fromErrorCode e
    header = FrameHeader 4 encodeFlags encodeStreamId

buildFramePayloadSettings
    :: EncodeInfo -> SettingsList -> IO (FrameHeader, Builder)
buildFramePayloadSettings EncodeInfo{..} alist = do
    settings <- create len $ \ptr -> go ptr alist
    let builder = (settings :)
    return (header, builder)
  where
    go _ [] = return ()
    go p ((k, v) : kvs) = do
        N.poke16 (fromSettingsKey k) p 0
        N.poke32 (fromIntegral v) p 2
        go (p `plusPtr` 6) kvs
    len = length alist * 6
    header = FrameHeader len encodeFlags encodeStreamId

buildFramePayloadPushPromise
    :: EncodeInfo -> StreamId -> HeaderBlockFragment -> (FrameHeader, Builder)
buildFramePayloadPushPromise einfo sid hdr = buildPadding einfo builder len
  where
    builder = (b4 :) . (hdr :)
    b4 = N.bytestring32 $ fromIntegral sid
    len = 4 + BS.length hdr

buildFramePayloadPing :: EncodeInfo -> ByteString -> (FrameHeader, Builder)
buildFramePayloadPing EncodeInfo{..} odata = (header, builder)
  where
    builder = (odata :)
    header = FrameHeader 8 encodeFlags encodeStreamId

buildFramePayloadGoAway
    :: EncodeInfo -> StreamId -> ErrorCode -> ByteString -> IO (FrameHeader, Builder)
buildFramePayloadGoAway EncodeInfo{..} sid e debug = do
    b8 <- create len0 $ \ptr -> do
        N.poke32 (fromIntegral sid) ptr 0
        N.poke32 (fromErrorCode e) ptr 4
    let builder = (b8 :) . (debug :)
    return (header, builder)
  where
    len0 = 8
    len = len0 + BS.length debug
    header = FrameHeader len encodeFlags encodeStreamId

buildFramePayloadWindowUpdate
    :: EncodeInfo -> WindowSize -> (FrameHeader, Builder)
buildFramePayloadWindowUpdate EncodeInfo{..} size = (header, builder)
  where
    -- fixme: reserve bit
    builder = (b4 :)
    b4 = N.bytestring32 $ fromIntegral size
    header = FrameHeader 4 encodeFlags encodeStreamId

buildFramePayloadContinuation
    :: EncodeInfo -> HeaderBlockFragment -> (FrameHeader, Builder)
buildFramePayloadContinuation EncodeInfo{..} hdr = (header, builder)
  where
    builder = (hdr :)
    len = BS.length hdr
    header = FrameHeader len encodeFlags encodeStreamId

buildFramePayloadUnknown :: EncodeInfo -> ByteString -> (FrameHeader, Builder)
buildFramePayloadUnknown = buildFramePayloadData
