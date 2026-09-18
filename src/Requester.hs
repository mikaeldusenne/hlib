{-# LANGUAGE OverloadedStrings #-}
module Requester where

import Network.HTTP.Conduit
import Data.ByteString.Lazy.Char8 (unpack)

data Method = GET | POST

build :: Method -> String -> RequestBody -> IO Request
build GET url _ = parseRequest url
  >>= (\request -> return request {method="GET"})
build POST url body = parseRequest url
  >>= (\nakedReq -> return nakedReq {method="POST",
                                    requestBody=body})
  

manager = newManager tlsManagerSettings

send :: Request -> IO String
send request = unpack.responseBody
  <$> (manager >>= httpLbs request)

