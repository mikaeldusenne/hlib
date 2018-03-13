{-# LANGUAGE TemplateHaskell,OverloadedStrings #-}
module DF where

import CSV
import Codec.Xlsx
import qualified Data.ByteString.Lazy as BL
import Data.Map hiding (map,foldl,foldr,filter,take,(!?))
import List
import Control.Lens
import General(fromJust, just_or_default)
import Tuple
import Data.Text (unpack)
import Stats(freqs, µ)
import Data.List(foldl')
import qualified Matrix
import Maths
import Text.Read(readMaybe)
-- import Text.ParserCombinators.Parsec

type Point = (Int, Int)

data Type = String' | Num'


data DF = DF {df'titles::[String],
              df'types ::[Type],
              df'data  ::CSV}

data Col = Col {dfc'title::String,
                dfc'type ::Type,
                dfc'data ::CSV'col}

types :: [Type] -> DF -> DF
types l (DF {df'titles=a, df'types=b, df'data=c}) =
  DF {df'titles=a, df'types=l, df'data=c}

column'wise :: DF -> [Col]
column'wise (DF titles types csv) = map dfcol $ zip3 titles types csv'
  where csv' = transpose csv
        dfcol = (\(a,b,c) -> Col a b c)
        
instance Show Type where
  show String' = "^String^"
  show Num'    = "^Numeric^"

               
instance Show DF where
  show df = showTableWithHeaders True $ cols
    where cols = map c $ column'wise df
            -- where c (Col t t' d) = t : show t' : d
            where c (Col t t' d) = t : d -- NO type shown
          -- wall (a:b:xs) = a:b:w:xs
          --   where w = take (count a) $ repeat '-'

readXlsx s = (fromJust . (^? ixSheet "Sheet1") . toXlsx)
  <$> BL.readFile s

-- determine the minimum / maximum row / column of a worksheet
wsRange :: Worksheet -> (Point,Point)
wsRange ws = ((top, left), (bottom, right))
  where [top,left,bottom,right] =
          (uncurry most<$>) $
          (,) <$> [reduce min,reduce max] <*> [fst,snd]
        most f which = f . ((which.fst)<$>) $ l
        l = toList $ ws ^. wsCells

(!?) :: Ord a => Map a b -> a -> Maybe b
(!?) m k = if k `member` m then Just (m!k) else Nothing

csv'to'DF :: Bool -> CSV -> DF
csv'to'DF headersyn csv = DF titles types csv'
  where titles = if headersyn
                 then head csv
                 else show <$> [1..]
        types = repeat String'
        csv' = if headersyn then tail csv else csv

ws'to'DF :: Worksheet -> DF
ws'to'DF ws = DF titles types l²
  where cm = ws^.wsCells
        cellContent r c = just_or_default "" $ cellToString <$> (cm !? (r,c) >>= (^.cellValue))
        types = take (count titles) $ repeat String'
        ((top,left),(bottom,right)) = wsRange ws
        (titles:l²) = [[ cellContent rn cn | cn <- [left..right]] | rn <- [top..bottom]]
--        l² = [[ cellContent rn cn | cn <- [left..right]] | rn <- [top..bottom]]
        

cellToString (CellText s)   = unpack s
cellToString (CellDouble d) = show d
cellToString (CellBool b)   = show b
cellToString (CellRich l)   = flatmap show l



--group'rows :: Show a => (String -> a)  -> DF -> DF
group'by indexf sep (DF a b c) = DF a b c'
  where c' = group'by' indexf sep c

-- we group the table by identifiant
-- then we "compress" each table
-- meaning that we "concatenate" each of it's column
-- meaning that we concat them with 'sep' and removig empty / duplicates
group'by' indexf sep = map compress . groupBy comp
        -- comp :: (Ord a) => [a] -> [a] -> Ordering
  where comp = (curry $ (uncurry compare)
                . applyToTuple indexf)
        compress = map concatenate . transpose
          where concatenate = concatWith sep
                              . uniq
                              . filter (not.isEmpty)



-- readColumn String' = id
readNum'    = fmap (readMaybe::String -> Maybe Double)

isNothing Nothing = True
isNothing _ = False

cleanCol :: [Maybe α] -> [α]
cleanCol = map (fromJust) . filter (not . isNothing)

-- readMaybe :: Read a => String -> Maybe a
-- readMaybe s = case reads s
--   of [(e,"")] -> Just e
--      _        -> Nothing

summary :: DF -> String
summary = concatWith "\n----\n" . map summarize . column'wise
  where summarize (Col ti ty d) = ti ++ "("++show ty++")"++ (f ty d)
          where f String' = ("\n\n"++)
                  -- . concatWith "\n    ) " . map show
                  . Matrix.showTable
                  . (\(a,b)-> [a,map show b]) . unzip . freqs
                f Num' = ("\n\n"++) . show . µ . cleanCol . readNum' 



-- column-first table from a <Char>-separated string
-- parsecsv :: Char -> String -> CSV
-- parsecsv csv_sep str = transpose $ p [] [] [] str False
--   where p table [] [] [] q = table
--         p table line [] [] q = p (table++[line]) [] [] [] q
--         p table line cell [] q = p table (line++[cell]) [] [] q
--         p table line cell s@(x:xs) False
--           | x=='\n'    = p (table ++ [line ++ [cell]]) [] [] xs False
--           | x==csv_sep = p table (line++[cell]) [] xs False
--         p table line cell ('"':'"':xs) quoted =
--           p table line (cell++"\"") xs quoted
--         p table line cell ('"':xs) quoted =
--           p table line cell xs (not quoted)
--         p table line cell (x:xs) quoted =
--           p table line (cell++[x]) xs quoted


-- parseCSV sep s = parselines s []
--   where parselines "" acc = acc
--         parselines s acc  = parselines s' $ acc ++ [l]
--           where (l,s') = parsecells s []
--                 parsecells "" acc = (acc,"")
--                 parsecells ('\n':xs) acc  = (acc,xs)
--                 parsecells s@('"':xs) acc = parsecells s' $ acc ++ [c]
--                   where (c,s') = untilquote s ""
--                         untilquote s@('"':'"':xs) acc = untilquote xs (acc ++ "\"\"")
--                         untilquote ('"':sep:xs) acc = (acc,xs)
--                         untilquote (x:xs) acc = untilquote xs (acc ++ [x])
--                 parsecells s acc = parsecells s' $ acc ++ [c]
--                   where (c,s') = untilsep s []
--                         untilsep "" acc = (acc, "")
--                         untilsep (x:xs) acc
--                           | x == sep  = (acc, xs)
--                           | otherwise = untilsep xs $ acc ++ [x]
--                           where is_end_of_field = (`elem`",\n")
  
-- csv_to_df :: [[String]] -> DF
-- csv_to_df t = DF (flatten names) cols
--   where (names, cols_str) = unzip . map (sliceN 1) $ t
--         cols = map p cols_str
--         p values
--           | all isNumber . concat $  values = CNb
--             $ map (\c -> if length c>0
--                          then Just (readDouble c)
--                          else Nothing) values
--           | otherwise = CStr . map Just $ values


