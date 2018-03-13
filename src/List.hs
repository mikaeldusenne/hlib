module List where
---- {-# LANGUAGE OverloadedStrings #-}
---- {-# LANGUAGE DeriveGeneric #-}


-- import GHC.Generics (Generic)
import qualified Data.List
import General
import Tuple
-- import Control.DeepSeq
-- import Data.Time.Clock
-- import Control.Parallel
-- import Control.Monad.Par
import Debug.Trace
import Data.List(isPrefixOf,foldl')
import Data.Char
import qualified Data.HashMap.Lazy as M
import Data.Hashable
-- count :: Num β => [α] -> β

-- count [] = 0
-- count (x:xs) = 1+count xs

-- tail recursion
-- count = count' 0
--   where count' k [] = k
--         count' k (x:xs) = count' (k+1) xs

---------------------------------------------------
{- SAFE -}

safe_head l = if isEmpty l then Nothing else Just (head l)
safe_tail l = if isEmpty l then Nothing else Just (tail l)

safe_nth 1 l      = safe_head l
safe_nth k []     = safe_nth 0 []
safe_nth k (x:xs) | k<1       = Nothing
                  | otherwise = safe_nth (k-1) xs



---------------------------------------------------

-- Functor Foldable
count :: (Num β, Foldable t, Functor t) => t α -> β
count = sum . ((\a -> 1) <$>)

-- isEmpty :: (Foldable t, Functor t) => t a -> Bool
-- isEmpty = (==0) . count
isEmpty :: [α] -> Bool
isEmpty [] = True
isEmpty _  = False

uncons (a:as) = (a,as)

-- zipLong [] (b:bs) = ([],b) : zipLong [] bs
-- zipLong (a:as) [] = (a,[]) : zipLong as []
-- zipLong (a:as) (b:bs) = (a,b) : zipLong as bs

-- zipLongWith f def [] (b:bs) = f [] b : zipLongWith f [] bs
-- zipLongWith f def (a:as) [] = f a [] : zipLongWith f as []
-- zipLongWith f def (a:as) (b:bs) = f a b : zipLongWith f as bs


makeAsLong fill la lb = lb ++ (replicate n fill)
  where n = (-) (count la) (count lb)

sumWith f = sum . map f
indexOf e = count . fst . sliceOn e

safe_indexOf e l = if isEmpty b then Nothing else Just (count a)
  where (a,b) = span (/=e) l


-- indexOf :: (Eq α,Integral β) => α -> [α] -> β
-- indexOf _ [] = Nothing
-- indexOf e (x:xs)
--   | e == x = 0
--   | otherwise = 1 + indexOf e xs


-- slice '/' "45/3"
-- -> ("45","3") 
slice :: (Eq α) => α -> [α] -> ([α],[α])
slice e = (\ (a,b) -> (a,
                       if count b > 1
                       then tail b
                       else []))
          . break (==e)


split :: (Eq α) => α -> [α] -> [[α]]
split _ [] = []
split sep l =
  let (a,b) = break (==sep) l in
  case b of
    [] -> [a]
    _ -> a : (split sep $ tail b)

-- last [] = error "empty list"
-- last [x] = x
-- last (x:xs) = List.last xs

-- last = reduce (\ a b -> b)

-- rev (x:xs) 

-- reduce _ [] = []
-- reduce _ [x] = x

-- reduce :: (t -> t -> t) -> [t] -> t
reduce _ [] = error "reduce empty list"
reduce _ [x] = error $ "reduce single element"
reduce f l = foldl' f (head l) (tail l)


flatten = reduce (++)
-- concat = flatten
-- flatmap f = (reduce (++)) . (map f)

--flatmap :: 
flatmap = (concat .) . map 

concatWith _ []  = []
concatWith _ [e] = e
concatWith e l = flatten . intercal e $ l

maxSize :: [[α]] -> Int
maxSize = reduce max . map count


allToMaxSize e l = if length l <= 1 then l else map (fix'size k e) l
  where k = maxSize l

-- allToMaxSize = all

-- flatmap f = reduce ( (++) . f )
-- flatmap :: ([α] -> [β]) -> [[α]] -> [β]
-- flatmap = reduce (++) . fmap
--flatmap = reduce . (((++).).) . map

-- intercal "|" ["ab","cd","e","fg"]
-- -> 
-- intercal e = reduce (\ a b -> a ++ e ++ b)
-- intercal e = reduce ((++) . (++e))
intercal :: α -> [α] -> [α]
intercal _ [] = []
intercal _ [x] = [x]
intercal e (x:xs) = x : e : intercal e xs

-- join :: [α] -> [[α]] -> [α]
-- join e = reduce ( (++) . (++e) )
-- joinList :: [α] -> [[α]] -> [α]
-- joinList sep l = concat . intercal sep $ l

beforeEach :: α -> [α] -> [α]
beforeEach e = ((!∫∫) id (e:) isEmpty) . intercal e

afterEach :: α -> [α] -> [α]
afterEach e = (++[e]) . intercal e


sliceN :: Int -> [α] -> ([α],[α])
sliceN = (`sliceN'`[])
  where sliceN' 0 a b = (reverse a,b)
        sliceN' _ a [] = sliceN' 0 a []
        sliceN' k a (x:xs) =
          sliceN' (k-1) (x:a) xs

-- headNtail (x:xs) = (x,xs)

-- sliceWhen :: ([a] -> Bool) -> [a] -> ([a],[a])
-- sliceWhen f = sl []
--   where sl acc [] = (acc,[])
--         sl acc (x:xs) | f x = (

-- sliceOn :: Eq α => α -> [α] -> ([α],[α])
-- sliceOn c = (\ (a,b) -> (a,tail b)) . span (/=c)

sliceOn :: Eq α => α -> [α] -> ([α],[α])
sliceOn c l = (a,b')
  where (a,b) = span (/=c) l
        b' = if isEmpty b then b else tail b



safe_sliceOn e l = if any (==e) l
                   then Just $ sliceOn e l
                   else Nothing

sliceWhen :: ([a] -> Bool) -> [a] -> ([a],[a])
sliceWhen f = sw []
  where f' l = isEmpty l || f l
        sw acc [] = (reverse acc,[])
        sw acc l@(x:xs) | f' l      = (reverse acc,l)
                        | otherwise = sw (x:acc) $ xs


car = head
cdr = tail
cadr = car . cdr
caddr = car . cdr . cdr
cadddr = car . cdr . cdr . cdr
cddr = cdr . cdr


dropEnd n l = take (count l - n) l
takeEnd n l = drop (count l - n) l 

--append x l = foldr (:) [x] l

splitEach :: Int -> [α] -> [[α]]
splitEach _ [] = []
splitEach k l = (take k l) : (splitEach k . drop k $ l)

insertBeforeEach e = concatMap (\a -> [e, a])

groupBy :: (Eq a,Ord a) => (a -> a -> Ordering) -> [a] -> [[a]]
groupBy f = foldl' insert []
  where insert acc e = walk acc
          where walk [] = [[e]]
                walk (l@(x:xs):ls) | f x e == EQ = append e l : ls
                                   | otherwise = l : walk ls

-- groupBy' :: (Eq b, Ord b,) => (a -> b) -> [a] -> M.HashMap b [a]
-- groupBy' f = foldl' addToMap M.empty
--   where addToMap m e = M.insertWith (++) (f e) [e] m
groupBy' :: (Hashable b, Eq b) => [b] -> [a] -> M.HashMap b [a]
groupBy' = (foldl' addToMap M.empty.) . zip
  where addToMap m (fe, e) = M.insertWith (++) fe [e] m


-- OMFG
-- groupBy defined as a fold!!
-- groupBy f = group -- . quickSortBy f
--   where group = foldl g []
--           where g [] a = [[a]]
--                 g ([]:bs) a = error "error b is empty!"
--                 g (b:bs) a
--                   | f a (head b) == EQ = ((a:b):bs)
--                   | otherwise    = ([a] : b : bs)




-- rewrite groupby if faster here?
-- splitFirstLetter :: (Ord a) => [[a]] -> [[[a]]]
-- splitFirstLetter = spltfst [] -- . quickSort
--   where spltfst acc l | trace ("splitFirstLetter" ++ (show $ count acc) ++ ";" ++ (show $ count l)) False = undefined
--         spltfst acc [] = acc
--         spltfst acc l@(x:xs) = spltfst (a:acc) r
--           where comp [] [] = EQ
--                 comp [] _ = LT
--                 comp _ [] = GT
--                 comp (a:as) (b:bs) = compare a b
--                 (a,r) = span ((==EQ) . (comp x)) l

splitFirstLetter :: (Ord a) => [[a]] -> [[[a]]]
-- splitFirstLetter = groupBy (\ a b -> (head a) `compare` (head b))
-- splitFirstLetter = groupBy ((.head) . compare . head)
-- splitFirstLetter l | trace ("splitFirstLetter " ) False = undefined
splitFirstLetter l = groupBy f l
  where f [] [] = EQ
        f [] _ = LT
        f _ [] = GT
        f (a:as) (b:bs) = compare a b

uniq l = uniq' l []
  where uniq' [] _ = []
        uniq' (x:xs) l
          | x `elem` l = uniq' xs l
          | otherwise  = x : uniq' xs (x:l)

-- uniq :: Eq α => [α] -> [α]
-- uniq = foldr f []
--   where f a [] = [a]
--         f a b = if a `elem` b
--                 then b
--                 else a:b
                  

-- splitOn :: ([a] -> Bool) -> [a] -> [[a]]
-- splitOn f l = splon l [[]]
--   where splon [] acc = reverse acc
--         splon l@(x:xs) acc@(a:as)
--           | f l = if isEmpty a
--                   then splon xs [[x]]
--                   else splon xs ([x]:acc)
--           | otherwise = splon xs ((a ++ [x]):as)



splitOn :: (α -> Bool) -> [α] -> [[α]]
splitOn _ [] = []
splitOn f l@(x:xs)
  | f x  = splitOn f xs
  | otherwise = foldr f' [[]] l
  where f' e acc@(a:as)
          | f e = ([]:acc)
          | otherwise = ((e:a) : as)



spansplit :: (a -> Bool) -> [a] -> [[a]]
spansplit f = reverse . foldl f' [[]]
  where f' acc@(a:as) c
          | f c       = [c]:acc
          | otherwise = ((a++[c]) : as)

separate :: (a -> Bool) -> [a] -> Maybe (a,[a])
separate f = f' []
  where f' acc (x:xs) | f x = Just (x,acc++xs)
                      | otherwise = f' (acc++[x]) xs
        f' acc [] = Nothing

nothingIf :: (a -> Bool) -> a -> Maybe a
nothingIf f x = if f x then Nothing else Just x 
                 
-- ss []
--   where ss acc [] = acc
--         ss acc (x:xs)
--           | f x = 


-- lines generalized
-- first arg is the list delimiter
splitWhen :: (Eq a) => [a] -> [a] -> [[a]]
splitWhen sep l = sp l [] []
  where sp [] acc s = acc++[s]
        sp l@(x:xs) acc s
          | beginWith sep l = sp (dropk l) (acc++[s]) []
          | otherwise       = sp xs        acc        (s++[x])
        dropk = drop (count sep)
        


contains :: Eq α => [α] -> [α] -> Bool
contains [] _ = True
contains _ [] = False
contains la@(a:as) lb@(b:bs)
  | a == b = check la lb || contains la bs
  | otherwise = contains la bs
  where check [] _ = True
        check _ [] = False
        check (a:as) (b:bs) = a == b && check as bs

append = (flip (++)) . (:[])

grep :: Eq a => [a] -> [[a]] -> [[a]]
grep = filter . contains


filterFirst f = head . filter f

spanList :: Eq α => ([α] -> Bool) -> [α] -> ([α], [α])
spanList f l = spnl l []
  where spnl [] _ = (l,[])
        spnl l@(x:xs) acc
          | f l = spnl xs (x:acc)
          | otherwise = (reverse acc , l)

rotate :: [α] -> [α]
rotate l = zipWith const (drop 1 (cycle l)) l

transpose :: [[α]] -> [[α]]
transpose ([]:_) = []
transpose l = a : transpose b
  where tuplecons (a,b) (c,d) = (a:c , b:d)
        (a,b) = foldr tuplecons ([],[])
                $ map headNtail $ l

startsWith :: (Eq α) => [α] -> [α] -> Bool
startsWith [] _ = True
startsWith l [] = False
startsWith (x:xs) (y:ys) = (x == y) && startsWith xs ys


-- rep '1' 4
-- -> "1111"
-- rep :: Integral n => α -> n -> [α]
-- rep a n = [ a | k <- [1..n] ]

-- repeat each element n number of times
repEach :: Int -> [α] -> [α]
-- repEach _ [] = []
-- repEach n (x:xs) = (rep x n) ++ repEach n xs
repEach n l = flatten $ map (replicate n) l

repList n l = flatten $ map (const l) [1..n]


quickSortBy :: Ord β => (α -> β) -> [α] -> [α]
quickSortBy _ [] = []
quickSortBy f (pivot:xs) = (qs α) ++ ( pivot : (qs β))
  where qs = quickSortBy f
        sort p [] τ π = (τ,π)
        sort p (x:xs) τ π
          | (f x) `compare` (f p) == LT    = sort p xs (x:τ) π
          | otherwise                      = sort p xs τ (x:π)
        (α,β) = sort pivot xs [] []


quickSort :: Ord α => [α] -> [α]
quickSort = quickSortBy id

------------------------------------------------

-- merge [] b = b
-- merge a [] = a
-- merge (a:as) (b:bs)
--   | compare a b == LT = a : merge as (b:bs)
--   | otherwise         = b : merge (a:as) bs


-- mergeSort :: (Ord α) => [α] -> [α]
-- mergeSort [] = []
-- mergeSort [x] = [x]
-- mergeSort l = merge a b
--   where (a',b') = splitAt ((count l) `div` 2) l
--         a = mergeSort a'
--         b = mergeSort b'

-------------------------------------------------

merge [] b = b
merge a [] = a
merge (a:as) (b:bs)
  | compare a b == LT = a : merge as (b:bs)
  | otherwise         = b : merge (a:as) bs


mergeSort :: (Ord α) => [α] -> [α]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort l = merge a b
  where (a',b') = splitAt ((count l) `div` 2) l
        a = mergeSort a'
        b = mergeSort b'


-- factorielle 0 = 1
-- factorielle n = n * factorielle (n-1)


----------
-- TEXT --
----------

-- letter
replace a b = fmap (\ e -> if e == a then b else e)

-- any
-- todo fold, tail recursion
replaceStr a b [] = []
replaceStr a b str | isPrefixOf a str = b ++ replaceStr a b (drop k str)
                   | otherwise = head str : (replaceStr a b $ tail str)
  where k = count a

dico = lines <$> readFile "/home/mika/Documents/random/dico_utf8_.txt" 

-- toLowerChar c = f alphalower alphaUpper
--   where f [] [] = c
--         f (l:ls) (u:us)
--           | c == l = c
--           | c == u = l
--           | otherwise = f ls us

-- this is in Data.List, but different
toLowerStr :: String -> String
toLowerStr = map toLower

beginWith :: Eq α => [α] -> [α] -> Bool
beginWith [] _ = True
beginWith _ [] = False
beginWith (a:as) (x:xs) = (a==x) && beginWith as xs


endWith end l = takeEnd (count end) l == end

after :: Eq α => [α] -> [α] -> [α]
after [] l = l
after begin [] = []
after begin l@(x:xs)
  | beginWith begin l = drop (count begin) l
  | otherwise = after begin xs


before :: Eq α => [α] -> [α] -> [α]
before b = reverse . after (reverse b) . reverse

between :: Eq α => [α] -> [α] -> [α] -> [α]
between a b = (before b) . (after a) 



(∩) :: Eq a => [a] -> [a] -> [a]
(∩) la lb = filter (`elem` lb) la
infixr 8 ∩

intersection :: Eq a => [a] -> [a] -> [a]
intersection = (∩)

(∪) :: Eq a => [a] -> [a] -> [a]
(∪) la lb = la ++ complement la lb


infixr 8 ∪

union :: Eq a => [a] -> [a] -> [a]
union = (∪)


-- complement "12" "123456" = "3456"
complement :: Eq a => [a] -> [a] -> [a]
complement l ω = filter (not . (`elem`l)) ω

---------------------------------------
--------- DICTIONARY ------------------
---------------------------------------




-- data DicoTree = E | T Char [DicoTree]
--  deriving (Show,Read)
-- type DicoForest = [DicoTree]

-- -- instance Show DicoTree where
-- --   show DicoEOT = "·"
-- --   show (DicoNode c l) = show c ++ show l

-- -- instance Read DicoTree where
-- --   reads = 

-- createTree :: [String] -> DicoForest
-- createTree l = map create .splitFirstLetter . Data.List.sort $ l
--   --where -- create a | trace ("create ") False = undefined
-- create ([]:xs) = E
-- create l@(x:xs) = T (head x)
--                   . map create
--                   . splitFirstLetter
--                   -- . filter (not . isEmpty)
--                   . map tail
--                   $ l

-- instance NFData DicoTree  where

--   rnf (T c l) = rnf c `seq` rnf l
--   rnf E = ()

-- createTreeParallel :: [String] -> DicoForest
-- createTreeParallel l | trace ("createTreeP " ++ (show . count $ l)) False = undefined
-- createTreeParallel l = do
--   let l' = splitFirstLetter . Data.List.sort  $ l
-- --      create a | trace ("create " ++ (show $ count a)) False = undefined
--       create [[]] = E
--       create l@([]:xs) = error $ "empty create!! " ++ (show l)
--       create l@(x:xs) = T (head x)
--                         . map create
--                         . splitFirstLetter
--                         . filter (not . isEmpty)
--                         . map tail
--                         $ l
--   runPar $ parMap create l'


-- -- AWESOME ?????
-- -- λ> writeFile "dicoForest" =<< show . createTree . lines <$> readFile "/home/mika/Documents/random/dico_utf8_.txt"
-- -- YES !!!!





-- isInDico :: DicoForest -> String -> Bool
-- isInDico [] _ = True
-- isInDico l w = any (==True)
--                . map (search w)
--                $ l
--   where search [] E = True
--         search w E = False
--         search [] _ = False
--         search (x:xs) (T c l) =
--           x == c
--           && (any (==True) . map (search xs) $ l)

-- computeFrequenciesFromDico :: String -> [(Char,Double)]
-- computeFrequenciesFromDico d = freqs letters
--   where freqs = map (\ (c,n) -> (c,n/total)) . foldl insert []
--         letters = flatten . lines $ d
--         insert [] c = [(c,1)]
--         insert (e@(char,n):xs) c
--           | char == c = ((char,n+1):xs)
--           | otherwise = e : insert xs c
--         total = count letters


intToLetter :: Int -> Char
intToLetter = (alphalower !! ) . (`mod` 26)

generateListOfLetters n = map intToLetter [0..n]



--------- 99 exercises

-- 10 run-length encoding

rlencode :: (Eq α) => [α] -> [(Int, α)]
rlencode = foldr f []
  where f b [] = [(1,b)]
        f b l@((n,e):xs)
          | e == b    = ((n+1,e):xs)
          | otherwise = ((1,b):l)



----------------------------------------------

-- remove trailing newline
notrailingln [] = []
notrailingln ('\r':'\n':[]) = []
notrailingln ('\n':[]) = []
notrailingln (x:xs) = x : notrailingln xs


nonull = filter (not . (== '\0'))

none :: Foldable t => (α -> Bool) -> t α -> Bool
none = (not.) . any

allSame [] = True
allSame (x:xs) = all (==x) xs

except subl = filter (not.(`elem`subl))

trimWith :: (α -> Bool) -> [α] -> [α]
trimWith f = applyN 2 (dropWhile f . reverse)

-- trimsp = dropWhile (==' ') . reverse . dropWhile (==' ') . reverse
trimsp = trimWith (==' ')
trim = trimWith (`elem` " \t\n\r")

surround2 a b = (a++) . (++b)
surround a = surround2 a a

surroundWith s = (s++) . (++s)

underline s = s ++ "\n" ++ (take (length s) $ repeat '─')

-- ┤"├"

showTableWithHeaders :: Bool -> [[String]] -> String
showTableWithHeaders showH cols = 
  unlines
  . map concat
  . transpose
  . surround2 [beginSep] [endSep]
  . intercal  middleSep
  . map (f . allToMaxSize ' ')
  $ cols
  where sep :: String -> String -> String -> [String]
        sep a b c = map (:[]) $ a ++ "│"
                    ++ (if showH then b else "")
                    ++ (take (length . tail $ head cols)
                        $ repeat '│')
                    ++ c
        beginSep  = sep "┌" "├" "└"
        middleSep = sep "┬" "┼" "┴"
        endSep    = sep "┐" "┤" "┘"
        f (t : b) =  ([w, t']++)
                     . ((if showH then [w] else [])++)
                     . (++ [w]) $ b'
          where t' = surround " " t
                b' = map (surround " ") b
                w  = take (2+length t) $ repeat '─'

boxify :: String -> [String] -> String
boxify title content = showTableWithHeaders True [title : content]

-- boxify title content = unlines . filter (not.null) $ [up, t, w, c', down]
--   where wall a b = surround2 a b $ take n $ repeat '─'
--           where n = count t - 2
--         up   = wall "┌" "┐"
--         w    = wall "├" "┤"
--         down = wall "└" "┘"
--         c' = (if length c>0 then init else id) $ unlines c
--         (t: c) = (\(a:b) -> trim a : b) . map f . allToMaxSize . (title:) $ lines content
--           where f = surround2 "│ " " │"

nice'title s = unlines
  . surround2 [horiz "┌" "┐"] [horiz "└" "┘"] $ ls'
  where horiz a b = surround2 a b $ take n $ repeat '─'
          where n = count (head ls') - 2
        f = surround2 "│ " " │"
        ls' = map f . allToMaxSize ' ' . lines $ s

-- nice'title' ls = unlines $ [horiz "┌" "┐"
--                            , unlines $ map f ls'
--                            , horiz "└" "┘"]
--   where horiz a b = surround2 a b $ take n $ repeat '─'
--           where n = count (head ls') + 2
--         f = surround2 "│ " " │"
  --         ls' = allToMaxSize ls

-- nice'title = nice'title' . (:[])

fix'size'side side k e s = f . take k $ s
  where f = if side==1 then (mappend strfill) else (`mappend` strfill)
          where strfill = take (max 0 (k-count s)) (repeat e) 

fill'left = fix'size'side 1
fill'right = fix'size'side 0

-- fix'size k e s = take k s ++ take (max 0 (k-count s)) (repeat e)
fix'size = fix'size'side 0

basename = last . filter (not . isEmpty) . splitOn (=='/')
