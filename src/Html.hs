module Html where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map, toList, fromList)
import List(surround2)

type Attributes = Map String String
type Tag = String

m = M.empty

data Element = E {_tag::Tag,
                  _class::[String],
                  _attrs::Attributes,
                  _elementl::[Element]}
               | Text String

tag'o = surround2 "<" ">"
tag'c = surround2 "</" ">"

instance Show Element where
  show (E t c a l) = unwords . filter ((>0).length) $
    [tag'o . unwords $ t : map disp (toList $ addClass a)
    ,unwords $ map show l
    ,tag'c t]
    where disp (k, v) = k ++ "=" ++ show v
          addClass = M.insert "class" (unwords c)
  show (Text s) = s

img = img' []
img' clss attrz = E "img" clss attrz []

div = div' []
div' clss = E "div" clss m

tag s = E s [] m

table :: [[String]] -> Element
table [] = E "table" [] m []
table (x:xs) = E "table" [] m $ (th x:map td xs)

th = E "tr" [] m . map (E "th" [] m . (:[])) . map Text
td = E "tr" [] m . map (E "td" [] m . (:[])) . map Text
