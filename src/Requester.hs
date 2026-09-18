{-# LANGUAGE OverloadedStrings #-}
-- | Small HTTP helpers. build constructs GET/POST requests without sending
-- them. send creates a manager per call; sendWith allows connection reuse.
-- Responses use a byte-to-Char mapping rather than a text decoder.
module Requester where

import Network.HTTP.Conduit
import Data.ByteString.Lazy.Char8 (unpack)

data Method = GET | POST

-- | Parse a URL and set the method. GET ignores the supplied body; POST uses it.
-- Invalid URLs throw an HTTP exception. No network request is made here.
build :: Method -> String -> RequestBody -> IO Request
build GET url _ = parseRequest url
  >>= (\request -> return request {method="GET"})
build POST url body = parseRequest url
  >>= (\nakedReq -> return nakedReq {method="POST",
                                    requestBody=body})
  

-- | Create a manager. Bind it once and use 'sendWith' to reuse connections.
manager :: IO Manager
manager = newManager tlsManagerSettings

send :: Request -> IO String
send request = manager >>= (\m -> sendWith m request)

-- | Send using a reusable manager. Like 'send', this preserves the legacy
-- byte-to-'Char' mapping; it does not decode UTF-8 or other text encodings.
sendWith :: Manager -> Request -> IO String
sendWith m request = unpack . responseBody <$> httpLbs request m
