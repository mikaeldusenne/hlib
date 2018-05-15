module QuotedPrintable 
       ( encode -- :: String -> String
       ) where

import Bases
import List
import Misc ((?))

import Data.Char
import Data.List(foldl')

-- converts a sequence of characeter into
-- quoted-printable form;
-- Conversion to UTF-8 is performed.
-- Soft line breaks are inserted if a line exceeds a max length.
encode :: String -> String
encode = concatMap (++"=\r\n") . reverse . foldl' enc [[]]
  where enc (x:xs) '\n' = (x++"\r\n") : xs
        enc l@(x:xs) c = appd $ if length x + length s >= 72
                                then []:l
                                else l
          where appd (a:as) = (a++s) : as
                s = ((char'to'qp ? (:[])) to'be'encoded) $ c
                  where to'be'encoded = o < 33 || o > 127 || elem c "=\t\n"
                        o = ord c



bit'headers = [0,192,224,240] 
bit'continuation = 128

-- maximum value for each number of bytes for utf-8 encoding
max'ord = fmap (2^) . (7 :) $ (\k -> (8-1-k) + 6*(k-1) ) <$> [2..]

-- number of bytes needed for this k = (ord Char)
char'utf8'categ k = length . takeWhile (k>) $ max'ord

-- encode it in utf8
char'to'utf8'bytes c = let
  o = ord c
  categ = char'utf8'categ o
  headers = bit'headers !! categ : replicate categ bit'continuation
  bodies = reverse $ f o
    where f k | k < 2^(8-categ-1) = [k] -- detect when in header byte
              | otherwise = b : f a
            where (a, b) = divMod k n
          n = 64
  in zipWith (+) headers bodies

-- get qp String
char'to'qp = concat . map (('=':) . f) . char'to'utf8'bytes
  where f k = concat . map to'hex $ [a, b]
          where (a, b) = divMod k 16
