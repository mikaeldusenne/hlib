module Colors where

import List
import Bases
import Maths
import Tuple

type ColorC = Fraction

data Color = Color { get'red  ::ColorC,
                     get'green::ColorC,
                     get'blue ::ColorC}
  deriving Show

-- todo                           
readN :: (Num a, Eq a) => a -> ReadS e -> ReadS [e]
-- readN 0 _ _ = []
readN k f = f' k []
  where f' 0 acc s = [(reverse acc,s)]
        f' k acc s = case f s of [] -> []
                                 [(e,xs)] -> f' (k-1) (e:acc) xs


instance Read Color where
  readsPrec _ r = [(Color r g b,xs)
                  | ("#", s) <- lex r,
                    ([r,g,b], xs)  <- readN 3 rhc s]
                  ++
                  [(Color r g b, xs)
                  | ("rgb", '(':s  ) <- lex r,
                    (r    , ',':s' ) <- rdc s,
                    (g    , ',':s'') <- rdc s',
                    (b    , ')':xs ) <- rdc s'']
-- rhc :: String -> [(Fraction,String)]
    where rhc = map (applyToFst (\[a,b] -> (/255) . toFraction $ a*16+b)) . readN 2 (reads'digit'base 16)
          rdc = map (applyToFst ((/255) . toFraction)) . (reads :: ReadS Int) 

color'toSomething :: (ColorC -> String) -> String -> String -> String -> Color -> String
color'toSomething f inter before after (Color r g b) = (before++) . (++after) . concatWith inter . map f $ [r,g,b] 

color'toHex = color'toSomething (xpd . to'hex . colorc'to 255) ""  "#"    ""
  where xpd [x] = '0':[x]
        xpd (x:y:_) = [x,y]
color'toRgb = color'toSomething (to'dec . colorc'to 255) "," "rgb(" ")"
color'toPercent = color'toSomething ( to'dec . (`div` 65535) . (100*) . colorc'to 65535) "," "%(" ")"

colorc'to k = round . fromFraction . (*k)

toList (Color r g b) = [r,g,b]
fromList [r,g,b] = Color r g b
