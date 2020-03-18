module Network.HTTP2.Arch.Object where

import Data.ByteString.Builder (Builder)
import Data.IORef (IORef)
import qualified Network.HTTP.Types as H

import Imports
import Network.HPACK
import Network.HTTP2.Frame
import Network.HTTP2.Arch.File

----------------------------------------------------------------

type InpBody = IO ByteString

data OutBody = OutBodyNone
             -- | Streaming body takes a write action and a flush action.
             | OutBodyStreaming ((Builder -> IO ()) -> IO () -> IO ())
             | OutBodyBuilder Builder
             | OutBodyFile FileSpec


-- | Input object
data InpObj = InpObj {
    inpObjHeaders  :: HeaderTable   -- ^ Accessor for headers.
  , inpObjBodySize :: Maybe Int     -- ^ Accessor for body length specified in content-length:.
  , inpObjBody     :: InpBody       -- ^ Accessor for body.
  , inpObjTrailers :: IORef (Maybe HeaderTable) -- ^ Accessor for trailers.
  }

instance Show InpObj where
    show (InpObj (thl,_) _ _body _tref) =
        "Response " ++ show thl ++ " "

-- | Output object
data OutObj = OutObj {
    outObjHeaders  :: [H.Header]    -- ^ Accessor for header.
  , outObjBody     :: OutBody       -- ^ Accessor for outObj body.
  , outObjTrailers :: TrailersMaker -- ^ Accessor for trailers maker.
  }

-- | Trailers maker. A chunks of the response body is passed
--   with 'Just'. The maker should update internal state
--   with the 'ByteString' and return the next trailers maker.
--   When response body reaches its end,
--   'Nothing' is passed and the maker should generate
--   trailers. An example:
--
--   > {-# LANGUAGE BangPatterns #-}
--   > import Data.ByteString (ByteString)
--   > import qualified Data.ByteString.Char8 as C8
--   > import Crypto.Hash (Context, SHA1) -- cryptonite
--   > import qualified Crypto.Hash as CH
--   >
--   > -- Strictness is important for Context.
--   > trailersMaker :: Context SHA1 -> Maybe ByteString -> IO NextTrailersMaker
--   > trailersMaker ctx Nothing = return $ Trailers [("X-SHA1", sha1)]
--   >   where
--   >     !sha1 = C8.pack $ show $ CH.hashFinalize ctx
--   > trailersMaker ctx (Just bs) = return $ NextTrailersMaker $ trailersMaker ctx'
--   >   where
--   >     !ctx' = CH.hashUpdate ctx bs
--
--   Usage example:
--
--   > let h2rsp = responseFile ...
--   >     maker = trailersMaker (CH.hashInit :: Context SHA1)
--   >     h2rsp' = setResponseTrailersMaker h2rsp maker
--
type TrailersMaker = Maybe ByteString -> IO NextTrailersMaker

-- | TrailersMake to create no trailers.
defaultTrailersMaker :: TrailersMaker
defaultTrailersMaker Nothing = return $ Trailers []
defaultTrailersMaker _       = return $ NextTrailersMaker defaultTrailersMaker

-- | Either the next trailers maker or final trailers.
data NextTrailersMaker = NextTrailersMaker TrailersMaker
                       | Trailers H.ResponseHeaders

-- | HTTP/2 push promise or sever push.
--   Pseudo REQUEST headers in push promise is automatically generated.
--   Then, a server push is sent according to 'promiseResponse'.
data PushPromise = PushPromise {
    -- | Accessor for a URL path in a push promise (a virtual request from a server).
    --   E.g. \"\/style\/default.css\".
      promiseRequestPath :: ByteString
    -- | Accessor for response actually pushed from a server.
    , promiseResponse    :: OutObj
    -- | Accessor for response weight.
    , promiseWeight      :: Weight
    }

----------------------------------------------------------------

-- | File specification.
data FileSpec = FileSpec FilePath FileOffset ByteCount deriving (Eq, Show)
