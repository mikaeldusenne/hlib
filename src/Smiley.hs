module Smiley where

-- import Control.Applicative ((<|>), many)

import Misc
import CSV
import List

-- newtype Smiley = Smiley {runSmiley :: String}

-- delimiters = oneOf alpha_num

-- parseSmiley :: String -> Either ParseError String
-- parseSmiley = parse p ""
--   where p = (try end <|> smiley <|> oneChar)
--         end = eof >> return ""
--         smiley = string "<3" >> return ":heart:" 
--         oneChar = (:[]) <$> anyChar

parseSmiley :: [([Char], [Char])] -> [Char] -> [Char]
parseSmiley slist = go True ""
  where go _ acc "" = reverse acc
        go ok acc s@(x:xs) = case ok of
          False -> f
          True  -> case subst s slist of
            ("", _) -> f
            (a, b)  -> go True (reverse a++acc) b
          where f = go (isOk s) (x:acc) xs
                isOk = or
                       . (<$>[null, not . (`elem` l) . head])
                       . (flip ($))
                  where l = '_':'-':alpha_num
                subst str [] = ("", str)
                subst str ((smiley, replacement) : smileys)
                  | beginWith smiley str && isOk b = (replacement, b)
                  | otherwise = subst str smileys
                  where b = drop (length smiley) str

-- escapespecialRe = f ".[{()*+?|^$" -- NB backslash missing
--   where f [] s = s
--         f (x:xs) s = f xs $ replaceStr [x] ("\\"++[x]) s

smileyList :: IO [([Char], [Char])]
smileyList = f <$> readFile "/home/mika/.perso/emoji_correspondance.csv"
  where f = map addstuff . parseCSV ','
        addstuff [a, b, c] = (a, ':' : b ++ ":")
        addstuff _ = error "smiley parsing error"

smileyList' :: IO [([Char], [Char])]
smileyList' = f <$> readFile "/home/mika/.perso/emoji_correspondance.csv"
  where f = map (\[a, _, c] -> (a, c)) . parseCSV ','

-- prepare_smileys_re' = map f
--   where f [a, b] = (g . escapespecialRe $ a, h b)
--         h = surround2 "\\1" "\\2"
--         g = surround2 (r "^") (r "$")
--           where r e = "(" ++ e ++ "|[^a-zA-Z0-9])"

-- prepare_smileys_re = map (applyToFst mkRegex) . prepare_smileys_re'

-- aa :: [([Char], [Char])] -> (a -> [Char]) -> a -> [Char]


substitute_smileys :: [Char] -> IO [Char]
substitute_smileys body = do
  l <- smileyList'
  return . parseSmiley l $ body


-- substitute_smileys body = do
  -- smileys <- prepare_smileys_re <$> smileyList
  -- smileys <- smileyList
  -- putStrLn . unlines . map show $ smileys
  -- return $ foldl' f body smileys
  --   where f :: String -> (Regex, String) -> String
  --         f s (smiley, replacement) = subRegex smiley s replacement
  

-- substitute_smileys body = do
--   smileys <- smileyList
--   let f "" _ acc = acc
--       f (x:xs) False acc = f xs (isBoundary x) $ acc ++ [x]
--       f s@(x:xs) True acc = f s' (isBoundary $ last s'') (acc++s'')
--         where (s'',s') = g smileys
--               g [] = ([x],xs)
--               g ([a, b]:xs) | beginWith a s && headBoundary s' = (b, s')
--                             | otherwise = g xs
--                 where s' = drop (length a) s
--       headBoundary "" = True
--       headBoundary (x:xs) = isBoundary x
--       isBoundary = (`elem` alpha_num)
--         -- (==Nothing) . matchRegex re . (:[])
--       -- re = mkRegex "[a-zA-Z0-9]"
--   return $ f body True ""

-- main = getContents >>= substitute_smileys >>= putStrLn
