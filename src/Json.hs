-- | Aeson 2 value helpers. Prefer lookupMaybe and unStringMaybe for external
-- input; the original lookup and unString functions remain partial.
module Json where

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson
import qualified Data.Text as T
import Misc


lookup :: T.Text -> Value -> Value
lookup k = fromJust . lookupMaybe k

-- | Look up an object member, returning 'Nothing' for a missing key or a
-- non-object value. Prefer this to the partial legacy 'lookup'.
lookupMaybe :: T.Text -> Value -> Maybe Value
lookupMaybe k (Object o) = KM.lookup (Key.fromText k) o
lookupMaybe _ _ = Nothing

unString :: Value -> String
unString (String s) = T.unpack s

-- | Extract a JSON string without throwing on other JSON types.
unStringMaybe :: Value -> Maybe String
unStringMaybe (String s) = Just (T.unpack s)
unStringMaybe _ = Nothing
