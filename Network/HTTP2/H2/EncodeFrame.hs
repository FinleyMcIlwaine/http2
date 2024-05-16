module Network.HTTP2.H2.EncodeFrame where

import Network.HTTP2.Frame

import Imports

----------------------------------------------------------------

goawayFrame :: StreamId -> ErrorCode -> ByteString -> IO ByteString
goawayFrame sid etype debugmsg = encodeFrame einfo frame
  where
    einfo = encodeInfo id 0
    frame = GoAwayFrame sid etype debugmsg

resetFrame :: ErrorCode -> StreamId -> IO ByteString
resetFrame etype sid = encodeFrame einfo frame
  where
    einfo = encodeInfo id sid
    frame = RSTStreamFrame etype

settingsFrame :: (FrameFlags -> FrameFlags) -> SettingsList -> IO ByteString
settingsFrame func alist = encodeFrame einfo $ SettingsFrame alist
  where
    einfo = encodeInfo func 0

pingFrame :: ByteString -> IO ByteString
pingFrame bs = encodeFrame einfo $ PingFrame bs
  where
    einfo = encodeInfo setAck 0

windowUpdateFrame :: StreamId -> WindowSize -> IO ByteString
windowUpdateFrame sid winsiz = encodeFrame einfo $ WindowUpdateFrame winsiz
  where
    einfo = encodeInfo id sid
