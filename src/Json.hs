module Json where

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson
import qualified Data.Text as T
import Misc


lookup :: T.Text -> Value -> Value
lookup k (Object o) = fromJust $ KM.lookup (Key.fromText k) o
lookup _ _ = error "Json.lookup: not an object"

unString :: Value -> String
unString (String s) = T.unpack s

