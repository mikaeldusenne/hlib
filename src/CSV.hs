module CSV where

import Data.Foldable
import Stats
import List
import Maths

-- list of rows
type CSV = [[String]]
type CSV'col = [String]

-- row first
parseCSV :: Char -> String -> CSV
parseCSV sep s = filter (not.emptyRow) $ p s [] [] []
  where appd l = (l ++) . (:[])
        -- appd l [] = l  -- wrong, this removes empty cells for no reason
        -- appd l e  = l ++ [e]
        emptyRow [] = True
        emptyRow [""] = True
        emptyRow _ = False
        
        p "" table line cell = (appd table (appd line cell))
        p ('"':xs) t l [] = p s' t l c'
          where (c',s') = quotedc xs []
                quotedc ('"':'"':xs) acc = quotedc xs $ acc ++ "\""
                quotedc ('"':xs) acc = (acc,xs)
                quotedc (x:xs) acc = quotedc xs $ acc ++ [x]
        p ('\n':xs) t l c = p xs (appd t (appd l c)) [] []
        p ('\r':'\n':xs) t l c = p xs (appd t (appd l c)) [] []
        p (x:xs) t l c
          | x == sep  = p xs t (appd l c) []
          | otherwise = p xs t l (c ++ [x])

seps = ",;"

guessSep :: String -> Maybe Char
guessSep = safe_head . f . lines
  where f l = map fst . quickSortBy (σ².snd) . filter ((>=1).µ.snd) $ foldl' g [] seps
          where g acc c = (c, map (toFraction.length . filter (==c)) l) : acc

-- csvFile = endBy line eol
-- line = sepBy cell (char ',')
-- cell = quotedCell <|> many (noneOf ",\n\r")

-- quotedCell = 
--     do char '"'
--        content <- many quotedChar
--        char '"' <?> "quote at end of cell"
--        return content

-- quotedChar =
--         noneOf "\""
--     <|> try (string "\"\"" >> return '"')

-- eol =   try (string "\n\r")
--     <|> try (string "\r\n")
--     <|> string "\n"
--     <|> string "\r"
--     <?> "end of line"

-- parseCSV :: String -> Either ParseError [[String]]
-- parseCSV input = parse csvFile "(unknown)" input

-- main =
--     do c <- getContents
--        case parse csvFile "(stdin)" c of
--             Left e -> do putStrLn "Error parsing input:"
--                          print e
--             Right r -> mapM_ print r

