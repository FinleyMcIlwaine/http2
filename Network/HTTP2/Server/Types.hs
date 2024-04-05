module Network.HTTP2.Server.Types where

import Network.Socket (SockAddr)
import qualified System.TimeManager as T

import Imports

----------------------------------------------------------------

-- | Server type. Server takes a HTTP request, should
--   generate a HTTP response and push promises, then
--   should give them to the sending function.
--   The sending function would throw exceptions so that
--   they can be logged.
type Server = Request -> Aux -> (Response -> [PushPromise] -> IO ()) -> IO ()

-- | Request from client.
newtype Request = Request InpObj deriving (Show)

-- | Response from server.
newtype Response = Response OutObj deriving (Show)

-- | HTTP/2 push promise or sever push.
--   Pseudo REQUEST headers in push promise is automatically generated.
--   Then, a server push is sent according to 'promiseResponse'.
data PushPromise = PushPromise
    { promiseRequestPath :: ByteString
    -- ^ Accessor for a URL path in a push promise (a virtual request from a server).
    --   E.g. \"\/style\/default.css\".
    , promiseResponse :: Response
    -- ^ Accessor for response actually pushed from a server.
    }

-- | Additional information.
data Aux = Aux
    { auxTimeHandle :: T.Handle
    -- ^ Time handle for the worker processing this request and response.
    , auxMySockAddr :: SockAddr
    -- ^ Local socket address copied from 'Config'.
    , auxPeerSockAddr :: SockAddr
    -- ^ Remove socket address copied from 'Config'.
    }
