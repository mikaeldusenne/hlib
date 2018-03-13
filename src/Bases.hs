module Bases where

import List
import Tuple
import qualified Data.ByteString.Lazy as B


charset'base 16 = map (:[]) $ ['0'..'9'] ++ ['A'..'F']
charset'base 64 = error "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="
charset'base n  = if n < 16
                  then take n $ charset'base 16
                  else map (('[':) . (++"]") . show) $ [0..(n-1)]

show'digit'base :: Int -> Int -> [Char]
show'digit'base = (!!) . (charset'base $)

reads'digit'base b s = if b <= 16
                       then if c `elem` charset
                            then [(indexOf c charset, xs)]
                            else []
-- indexOf ((:[]) . head $ s) $ charset'base b
                       else case s of ('[':xs) -> fst . head $ reads xs
                                      _ -> error "noooo parse"
  where (c,xs) = applyToFst (:[]) . headNtail $ s
        charset = charset'base b

read'digit'base b s = if b <= 16
                      then indexOf ((:[]) . head $ s) $ charset'base b
                      else case s of ('[':xs) -> fst . head $ reads xs
                                     _ -> error "noooo parse"


to'base 10 n = show n
to'base 64 b = error "todo"
to'base  b n | n == 0 || n == 1 = sdb n
             | otherwise =
               case divMod n b of (0,r) -> sdb r
                                  (q,r) -> to'base b q ++ to'base b r
                                  
  where sdb = show'digit'base b


to'bin = to'base 2
to'dec = to'base 10
to'hex = to'base 16
-- to'b64 = error "todo"

        
powersOf = (<$>[0..]) . (^)

from'base :: Int -> String -> Int
from'base 10 s     = read s
from'base 64 _     = error "todo"
from'base _ []     = 0
from'base b s      = sum
                     . zipWith (*) (powersOf b)
                     . reverse
                     . vs
                     $ s 
  where vs = if b > 16
             then map ((read::String -> Int) . tail) . init . splitWhen "]"
             else map (read'digit'base b . (:[])) 



-- { Base 64 Module }

-- to'b64 :: B.ByteString -> String
-- to'b64 bs = 

