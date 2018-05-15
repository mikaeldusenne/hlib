module Maths where

import Text.Printf hiding (map)
import List
import Data.Ratio (numerator,
                   denominator)
import qualified Data.Ratio ((%))
import Tuple
-- import Control.Parallel
import Misc
import Constants


---------------------
{- MODULE FRACTION -}
---------------------

-- module Fraction where

-- import Tree
-- import Maths
-- import List
-- import Data.Ratio (numerator,
--                    denominator)
-- import qualified Data.Ratio ((%))
-- import Tuple


type Numerator = Integer
type Denominator = Integer
data Fraction = Fraction {
  fracNumerator :: Numerator,
  fracDenominator :: Denominator }
 -- deriving (Read)

instance Show Fraction where
  show (Fraction a 1) = show a
  show (Fraction 0 _) = "0"
  show f@(Fraction a b) = show a' ++ "/" ++ show b'
    where (Fraction a' b') = simplify f
-- String representing a number ~= [0-9]\.
-- --> to Fraction
parseRationalToFraction :: String -> Fraction
parseRationalToFraction s = readInteger (a++b) % n
  where (a,b) = slice '.' s
        n = (10^) . count $ b

instance Read Fraction where
  readsPrec d str = readParen (d>1) fracFromInt str
                    ++ readParen (d>1) fracFromDiv str
    where fracFromInt s = [(parseRationalToFraction n,t)
                          | (n,t) <- lex s]
          fracFromDiv s = [(a%b,t) | (a,r) <- readsPrec 10 s
                                   , ("/",r') <- lex r
                                   , (b,t) <- readsPrec 10 r']
  
readFraction :: String -> Fraction
readFraction ('-':xs) = negate . readFraction $ xs
readFraction c
  | elem '/' c = parseFrac c
  | elem '.' c = parseDouble c
  | otherwise = parseInteger c
  where
    parseFrac = (uncurry (/))
                . applyToTuple readFraction
                . slice '/'
    parseDouble c = fa + fb
      where (a,b) = slice '.' c
            fa = (%1) . readInteger $ a
            fb = (readInteger b) % (fromIntegral
                                    . (10^)
                                    . count $ b)
    parseInteger = (%1) . readInteger 

instance Eq Fraction where
  (Fraction a b) == (Fraction c d) = a*d == b*c


instance Ord Fraction where
  compare (Fraction a b) (Fraction c d) =
    compare (a*d*(signum d)*(signum b)) (b*c*(signum d)*(signum b))

instance Num Fraction where
  (+) (Fraction a b) (Fraction c d) =
    simplify $ Fraction (a*d+c*b) (b*d)
  (*) (Fraction a b) (Fraction c d) =
    simplify $ Fraction (a*c) (b*d)
  abs (Fraction a b) = Fraction (abs a) (abs b) 
  signum (Fraction a b) = Fraction (signum (a*b)) 1
  fromInteger n = Fraction n 1
  negate (Fraction a b) = Fraction (-a) b

instance Fractional Fraction where
  (/) f = simplify . (*) f . recip
  recip (Fraction a b) = Fraction b a
  fromRational r = Fraction (numerator r) (denominator r)


instance Real Fraction where
  toRational (Fraction a b) = a Data.Ratio.% b

-- instance RealFrac Fraction where
--   properFraction (Fraction a b) = (n,f)
--     where n = 


roundFraction :: (Integral α) => Fraction -> α -> Fraction
roundFraction frac n = (/p) . toFraction . round . fromFraction . (*p) $ frac
  where p = 10^^n


--------------------------------------

(%) :: Numerator -> Denominator -> Fraction
(%) a = simplify . Fraction a
infixl 8 %
--infixr 8 %
--------------------------------------

fromFraction :: (Real β,Fractional α) => β -> α
fromFraction = fromRational . toRational

-- toFraction :: Show α => α -> Fraction
-- toFraction = readFraction . show
toFraction :: (Real α,Fractional β) => α -> β
toFraction = realToFrac

num :: Fraction -> Integer
num (Fraction n _) = n

denom :: Fraction -> Integer
denom (Fraction _ d) = d

simplify :: Fraction -> Fraction
simplify (Fraction 0 _) = Fraction 0 1
simplify (Fraction a b)
--  | b < 0 = simplify (Fraction (-a) (-b))
--  | otherwise
  = if b'>0
    then Fraction a' b'
    else Fraction (-a') (-b')
  where d = pgcd a b
        a' = div a d
        b' = div b d

fracPower :: (Real α,Fractional β) => α -> α -> β
fracPower a b = realToFrac $ (realToFrac a) ** (realToFrac b)

----------------------------
{- END OF MODULE FRACTION -}
----------------------------

----------------------------
{- MODULE MATHS -}
----------------------------


charToDigit = (`indexOf`digit)

readInt s = read s :: Int
readInteger s = read s :: Integer
readDouble s = read s :: Double
readFloat s = read s :: Float

printPrecision :: (Num α,Show α) => α -> Fraction -> String
printPrecision k x = printf format ((fromFraction x) :: Double)
  where format = "%." ++ show k ++ "f"



pgcd a b = let (q,r) = quotRem a b in
  if r == 0
  then b
  else pgcd b r

sumOfDigits :: Integer -> Integer
sumOfDigits n
  | n < 0 = sumOfDigits (-n)
  | n < 10 = n
  | otherwise = sumOfDigits . foldl fadd 0 . show $ n
  where fadd a = (+a) . charToDigit

divisibleBy2 :: Integer -> Bool
divisibleBy2 = (==0) . (`mod`2) . charToDigit . last . show

divisibleBy3 = (==0) . (`mod`3) . sumOfDigits



-- factors :: Integer -> [Integer]
-- factors 



fact :: Integer -> Integer
-- fact 0 = 1
-- fact n = (*n) . fact $ n-1
fact = fact' 1
  where fact' acc 0 = acc
        fact' acc n = fact' (n*acc) (n-1)


  
-- showChoose :: Num α => α -> α -> [[α]]
-- showChoose n 0 = []
-- showChoose n 1 = [ [x] | x <- [0..n-1]]
-- showChoose n k = flatmap (showChoose (n-1) (k-1))[0..n-1]

-- concat . intercal "\n" . map show .
showPermutations :: Show α => [α] -> String
showPermutations =  concat . intercal "\n" . map show . permuts

rotations l = rot l (count l)
  where rot l 0 = []
        rot l n = l : rot (rotate l) (n-1)


permuts :: [α] -> [[α]]
permuts [] = []
permuts [x] = [[x]]
permuts l = flatten
            . map (\ (a:as) -> map (a:) (permuts as) )
            $ rotations l
        
intersect :: Eq α => [[α]] -> [[α]] -> [[α]]
intersect la lb
  | count la > count lb = intersect lb la
  | otherwise = filter (`elem`lb) la


-- infixr 7 `choose`

-- sgn a
--   | a < 0 = -1
--   | otherwise = 1

-- truncating
-- -- euclidivPrecision :: Integer -> Integer -> Integer -> String
euclidivPrecision n (Fraction {fracNumerator=a,fracDenominator=b}) =
  (show q) ++ "." ++ r'
  where (q,r) = divMod a b
        div' 0 _ = []
        div' n a = (show q) ++ (div' (n-1) r)
          where (q,r) = divMod (a*10) b
        r' = div' n r


-- nfib :: Int -> Int
-- nfib n | n <= 1 = 1
--        | otherwise = par n1 (pseq n2 (n1 + n2 + 1))
--                      where n1 = nfib (n-1)
--                            n2 = nfib (n-2)
-- tail recursion
nfibt :: Int -> Int
nfibt n = f 0 1 0
  where f a b n' | n' >= n   = b
                 | otherwise = f b (a+b) $ n' + 1

nfib_std :: Int -> Int
nfib_std n | n <= 1 = 1
       | otherwise = 1 + (nfib_std (n-1)) + (nfib_std (n-2))


------ Nat

data Nat = Zero | Succ Nat
  deriving Show
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Nat
integerToNat n | n < 0 = error "no way dude"
integerToNat 0 = Zero
integerToNat n = Succ $ integerToNat (n-1)

instance Num Nat where
  (+) Zero b = b
  (+) a Zero = a
  (+) (Succ a) nb@(Succ b) = (+) a (Succ nb)

  (-) a Zero = a
  (-) Zero b = error "Negative Number"
  (-) (Succ a) (Succ b) = (-) a b

  (*) Zero _ = Zero
  (*) _ Zero = Zero
  (*) (Succ Zero) b = b
  (*) a (Succ Zero) = a
  (*) na@(Succ a) nb = nb + ((*) a nb)

  signum _ = 1

  abs a = a

  fromInteger = integerToNat



byte_units = ["kB","MB","GB","TB"]

prettyBytes :: Integer -> String
prettyBytes n = f (n % 1) byte_units
  where f k [x] = k' ++ " " ++ x
          where k' = if (fromInteger (round $ fromFraction k)) == k then show k else printPrecision 2 k
        f k (x:xs) | k<1024 = f k [x]
                   | otherwise = f (k/1024) xs
