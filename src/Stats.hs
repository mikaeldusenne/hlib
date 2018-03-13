module Stats where

-- import Fraction
import General
import Maths
import Constants
import Distribution_Tables

import List
import Matrix
import Control.Monad
import System.Random
import Tuple
import Text.Printf

import qualified Data.ByteString.Lazy as BL
import qualified Data.List
import Control.Monad.Trans.State
import System.Process
import System.IO
import Statistics.Distribution.FDistribution
import Numeric.SpecFunctions (incompleteBeta,invIncompleteBeta )
import Data.List(foldl')
------------------------- VALUES

path_data_stats = "/home/mika/Documents/Cours/Stats/data/"


------------------------- DATA TYPES



-- readDF s = 

data DataFrame α = DataFrame [Col α]
  -- deriving Show 


-- data DFCell = C String

-- instance Show DFCell where
--   show (C s) = s

-- instance Eq DFCell where
--   (C s) == (C s') = s==s'

-- instance Ord DFCell where
--   compare (C s) (C s') = compare s s'


-- instance Show a => Show (DataFrame a) where
--   show (DataFrame cols) = unlines . map (concatWith "|") . transpose . map prepareColumn $ cols
--     where prepareColumn (Col h c) = all_same_size . (h:) . map show $ c
--             where all_same_size l = map (fix'size k) l
--                     where k = reduce max $ map count l
                    
--   show'dataframe :: Matrix (Col String) -> String
-- show'dataframe (Matrix r c cells) =
--   showTable . transpose
--   . map (\(Col t c) -> [t]++c)
--   $ cells
  
------------------------- DATA CONSTRUCTORS

-- readTable sep = map (splitWhen sep) . lines

-- data DF = DF {
--   getStrings :: DFStr,
--   getNums    :: DFNum
--   }

-- data DFSth α = DFSth {getNames :: [String], getCols :: [C α]}
-- type DFStr = DFSth String
-- type DFNum = DFSth Double

-- data C α = C [Maybe α]


-- colToStrings :: C -> [String]
-- colToStrings (CStr values) = map jstr values
--   where jstr Nothing = "NA"
--         jstr (Just s) = s
-- colToStrings (CNb values) = map jnb values
--   where 
--         jnb Nothing = "NA"
--         jnb (Just n) = show n

-- -- not gonna happen
-- -- instance Functor C where
-- --   fmap f (CStr c) = CStr $ f <$> c
-- --   fmap f (CNb c) = CNb $ f <$> c

-- instance Show DF where
-- --  show (DF cols) = ""
--   show (DF names cols) = unlines . map (concatWith "|")
--                          . transpose $ t
--     where scol (title, values) = title : horizbar
--             : colToStrings values
--           horizbar = take 99 (repeat '-')
--           namesbar = map (\t -> [t] ++ [horizbar]) names
--           t = map fixCol $ zipWith (++) namesbar $ map colToStrings cols
-- --          t = map (fixCol . scol) cols -- [[String]]
--           fixCol c = map (fixedLength ' ' k) c
--             where k = foldr max 0 . map length . dropwall $ c
--                   dropwall (x:y:zs) = x:zs

-- -- instance Functor DF where
-- --   fmap f (DF df) = DF . f $ df

-- -- list to fixed length
-- -- fill with e's if too short,
-- -- cut the end if too long
-- fixedLength e n l = (take k $ repeat e) ++ take k' l 
--   where k = n - length l
--         k' = min (length l) n

-- -- column-first table from a <Char>-separated string
-- parsecsv :: Char -> String -> [[String]]
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

-- -- R like subsetting with boolean vectors
-- -- subdf :: [Bool] -> [Bool] -> DF -> DF
-- -- subdf r c (DF df) = DF . fc . fr $ df

-- fc b (DF names cols) = DF (f names) (f cols)
--   where f = filterBool b
-- -- fc bs df = foldr (\(b,v) -> \vals -> if b then v:vals else vals)
-- --         [] $ zip bs df

-- -- fr b (DF names cols) = DF names $ map (filterBool b) cols
-- --   where fboolcol =


-- filter values by a boolean vector
filterBool b l = foldl f [] $ zip b l
  where f l (b,e) = if b then l++[e] else l

-- fr bs df = 

-- table :: C -> T
-- table (CStr c) = 1
-- table (CNb c) = 1
          
-- weird function (similar to "by")
-- create DataFrame from a table [row]
-- by specifying how to get the factor
dataTableFromTable kfactor kdata l =
  cols
  where factorl = map (!!kfactor) l
        datal   = map (!!kdata) l
        tuples = zip factorl datal
        groups = groupBy comp tuples
          where comp a b = compare (fst a) (fst b)
        titles = map (fst . head) groups
        vals = map (map (readDouble . snd)) groups
        cols = zipWith Col titles vals

-- takes the first element as a header
-- assumes it is a list of columns, (not a list of rows)
tableToDF :: (String -> a) ->  [[String]] -> [Col a]
tableToDF f = map (\(h:c) -> Col h (map f c))

---------------- PROBABILITY

----- PRN

-- rollDie :: (Random a,Num a) => IO a
-- rollDie = randomRIO(1,6)

-- rollNdice n = mapM (\_ -> rollDie) [1..n]

-- rollNdice :: Int -> [Int]
-- rollNdice 0 = []
-- rollNdice n = rollDie 




-- rollDie :: State StdGen Int
-- rollDie = state $ randomR (1, 6)

rollDie :: State StdGen Int
rollDie = do
  generator <- get
  let (value,newGen) = randomR (1,6) generator
  put newGen
  return value



-- frd :: (Monad m) => (State ((),s))
-- frd = do
--   a <- get
--   put $ a
--   return ()

-------


----------- Probability

-- choose :: Integral α => α -> α -> Fraction
choose :: Integral α => α -> α -> α
--choose :: Integer -> Integer -> Integer
choose n k
  | n < k     = error "Choose n k"
  | otherwise = div a b
  where α = [1..n]
        β = [1..k] ++ [1..(n-k)]
        a = foldr (*) 1 α
        b = foldr (*) 1 β

showchooseInt :: Int -> Int -> [[Int]]
showchooseInt n k = showchooseList [0..n-1] k 

showchooseList :: [α] -> Int -> [[α]]
showchooseList _ 0 = []
showchooseList l 1 = map (:[]) l
showchooseList l k = flatmap f [drop x l | x <- [0..n-k]]
  where n = count l
        f (x:xs) = map (x:) (showchooseList xs (k-1))



-- binom :: (Integral α) => α -> Fraction -> α -> Fraction
-- binom :: (Fractional b, Integral b) => b -> b -> b -> b
-- binom :: (Num a,Integral b) => b -> a -> b -> a
binom n p k = fromIntegral (choose n k) * p^k * (1-p)^(n-k)

-- dbinom :: (Integral b, Fractional b) => b -> b -> [b]
dbinom n p = map (binom n p) [0..n]

-- cloppearson
--   :: (Show t, Fractional t, Integral t) => t -> Fraction -> [(t, Fraction)]

-- cloppearson :: Fraction -> Fraction -> [(Fraction, Fraction)]
cloppearson n p = let
  dbin = dbinom n p
  props = map (/fromInteger n) [0..(fromInteger n)]
  risk = 0.025
  l = zip props dbin

  f (pacc, acc) (p,b) = if acc+b>risk then (pacc,acc) else (p, acc+b)

  min = foldr f (0,0) l
  max = foldr f (0,0) . reverse $ l

  in [min,max]

-- dbinom :: (Integral α,Real β) => α -> β -> [Fraction]
-- dbinom n p = la ++ middle ++ reverse la
--   where (a,b) = (\(a,b) -> (fromInteger a,b)) $ divMod (toInteger n) 2
--         q=1-p
--         la = map f [0..a-1]
--         middle = map f $ if b == 1 then [] else [a]
--         f = binom n p
        -- something m = (choose n m *) . toFraction $  p^m * q^(n-m)

-- graph'dbinom :: (Integral α,Real β) => α -> β -> IO ()
-- graph'dbinom a b = f $ dbinom a b
--   where f l = putStrLn . ("\n"++) . unlines . map g $ l
--           where m = reduce max l
--                 -- g :: Fraction -> String
--                 g e = ((++) (fix'size 6 ' ' $ printPrecision 2 (e * 100))) . take k . repeat $ '#'
--                   where k = fromInteger . round $ k'
--                         k' =  fromFraction $ 29 * e / m

-- pbinom :: (Integral α, Real β) => α -> β -> Fraction
-- pbinom 



-- -- total = number of balls
-- -- goal  = number of "right" balls
-- data Lottery = Lottery {
--   total :: Integer,
--   goal  :: Integer}
--   deriving Show

-- -- compute the probability to get
-- -- k good balls
-- -- from a lottery
-- proba_lottery :: Lottery -> Integer -> Fraction
-- proba_lottery (Lottery n b) k =
--   choose b k * choose (n-b) (b-k) / choose n b

-- all_proba_lottery l = map (proba_lottery l) [0..g]
--   where g = goal l

-- pretty_all_proba_lottery l@(Lottery n b) =
--   putStrLn.unlines.map (printPrecision 15).all_proba_lottery $ l


random01 :: (RandomGen γ) => γ -> (Double, γ)
random01 = (\(a,b) -> (a :: Double, b)) . randomR (0,1)

-- for computation
sqrt2pi = sqrt $ 2 * pi
recipsqrt2pi = 1 / sqrt2pi


{- Normal Distribution -}


-- box-muller
-- normalr mean sd g = ([f cos, f sin],gb)
--   where (ra,ga) = random01 g
--         (rb,gb) = random01 ga
--         c = sqrt $ (-2) * log ra
--         f = (+mean) . (*sd) . (*c) . ($ 2 * pi * rb)

normalr :: (RandomGen g) => Double -> Double -> State g [Double]
normalr mean sd = do
  g <- get
  let (ra,g') = random01 g
      (rb,g'') = random01 g'
      c = sqrt $ (-2) * log ra
      f = (+mean) . (*sd) . (*c) . ($ 2 * pi * rb)
  put g''
  return [f cos, f sin]

-- rnorm mean sd k g 
--   | k <= 0    = ([],g)
--   | otherwise = ((take n la) ++ lb ++ lc,g''')
--   where n = min k 2
--         (k',r) = (k-2) `divMod` 2
--         k'' = k' + r
--         ( la  ,g')   = normalr  mean sd     g
--         ( lb  ,g'')  = rnorm mean sd k'  g'
--         ( lc  ,g''') = rnorm mean sd k'' g''

rnorm :: (RandomGen g) => Double -> Double -> Int -> State g [Double]
rnorm mean sd k
  | k <= 0 = return []
  | otherwise = do
      let n = min k 2
          (k',r) = (k-2) `divMod` 2
          k'' = k' + r
      ns <- normalr mean sd
      la <- rnorm mean sd k'
      lb <- rnorm mean sd k''
      return $ take n ns ++ la ++ lb

-- density function
dnorm x mean sd =
  ( recipsqrt2pi / sd )
  * e ** (negate ((x-mean)^2) / (2*sd^2))

  
-- approximation of the error function
-- http://www.johndcook.com/blog/python_phi/
erf x = signum x * y
  where 
    --  constants
    -- todo define out of function ? efficiency? no idea
    a1 =  0.254829592
    a2 = -0.284496736
    a3 =  1.421413741
    a4 = -1.453152027
    a5 =  1.061405429
    p  =  0.3275911

    x' = abs(x)

    -- A & S 7.1.26
    t = 1.0/(1.0 + p*x')
    y = 1.0 - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t* e ** (-x'*x')



-- Cody, W. D. (1993) Algorithm 715: SPECFUN – A portable FORTRAN package of special function routines and test drivers. ACM Transactions on Mathematical Software 19, 22–32
-- integral from -Inf
-- cumulative distribution of the normal function (cdf)
pnorm = (/2) . (+1) . erf . (/sqrt2)

-- Wichura, M. J. (1988) Algorithm AS 241: The percentage points of the normal distribution. Applied Statistics, 37, 477–484
qnorm = undefined

-- beta :: Double
-- beta x y = sqrt2pi * f x * f y / f (x+y)
--   where f e = e ^ (e-0.5)


-----------------------------
{-     TABLE GENERATOR     -}
-----------------------------


-- just like https://homes.cs.washington.edu/~jrl/normal_cdf.pdf
table_cumulative_normal_proba :: String
table_cumulative_normal_proba =
  unlines . addRowSep
  . map (concat . addColSep . intercal "\t" . map tfl)
  $ header : body
  where centi = (/100) <$> [0..9]
        range =  (/10) <$> [-34..34]
        header = ("z":) $ printPrecision 2 . toFraction <$> centi
        row x = 
          (printPrecision 1 . toFraction $ x) : 
          (printPrecision 4 . toFraction <$> (( pnorm . (add x))
                                              <$> centi))
        body = row <$> range
        add a b = signum a * (abs a + b)
        addColSep (x:y:xs) = x : "┃" : xs
        addRowSep (x:xs) = x : sep : xs
          where l = (`replicate` '━')
                sep = l 7 ++ ('╋' : l 80)
        -- toFixedLength 0 _      = []
        -- toFixedLength k ""     = toFixedLength (k-1) "" ++ " "
        -- toFixedLength k (x:xs) = x : toFixedLength (k-1) xs
        toFixedLength k s = (replicate (k - count s) ' ') ++ take (max k (count s)) s
        tfl = toFixedLength 7






-- Generate F-Distribution tables
-- from a call to Rscript, and R's "qf" function
table_qf_from_R_lol :: Fraction -> IO F_TABLE
table_qf_from_R_lol α =
  ((F_TABLE nums denoms α)<$>)
  $ sequence
  $ (`map`cmds)
  $ callRscript
  . combineLine
  
  where 
        αstr = printPrecision 4 $ 1-α
        nums   = [1..10] ++ [12,15,20,24,30,40,60,120,infinint]
        denoms = [1..30] ++ [40,60,120,infinint]
        -- cmds = gen_cmd <$> nums <*> denoms
        cmds = cmd_generator nums denoms
        combineLine l = "cat(" ++ concatWith [',','"',' ','"',','] l ++ ")" 

        gen_cmd num denom = "qf(" ++ αstr
                            ++ ", df1=" ++ show num
                            ++ ", df2=" ++ show denom
                            ++")"

        cmd_generator nums denoms =
          (<$>denoms) $ (\el -> fmap ($ e) (gen_cmd <$> nums))


--callRscript :: String -> IO [Double]
        callRscript s = 
          ((\(_,Just hout,_,_) -> hout)
           <$> createProcess (proc "Rscript" ["-e",s]){std_out=CreatePipe})
          >>= ((convert <$>) . hGetContents)
          where convert = map readDouble . words

f_distrib_alphas = [0.1,0.05,0.025,0.01,0.001] :: [Fraction]
io_f_distrib_tables = sequence $ table_qf_from_R_lol <$> f_distrib_alphas



-- -- returns the F critical value
-- -- for a given set of α , df num and df denom
-- -- Nothing if not in pre-computed tables
-- f_stat_critical :: Fraction -> Int -> Int -> Maybe Double
-- f_stat_critical α num denom = safe_head t >>= lookup num denom
--   where t = filter ((==α) . alpha) f_distribution_tables
--         lookup num denom (F_TABLE n d α v) = do
--           ni <- safe_indexOf num   n
--           di <- safe_indexOf denom d
--           return (v !! di !! ni)

f_stat_critical :: Fraction -> Int -> Int -> Maybe Double
f_stat_critical α' n' d' =
  let α = fromFraction α'
      n = fromIntegral n'
      d = fromIntegral d'
      y = n * α
  -- in Just $ incompleteBeta (0.5 * n) (0.5 * d) (y/(d+y))
      x = invIncompleteBeta (0.5 * n) (0.5 * d) α
  in Just $ d * x / (n * (1 - x))

-----------------------------
{- DESCRIPTIVE STATISTICS  -}
-----------------------------



-- mean :: (Fractional α,Foldable t) => t α -> α
-- -- mean l = (/) (count l) (sum l)
-- mean = uncurry (/) . foldr (\a (s,c) -> (s+a,c+1) ) (0,0)


-- sumSq :: (Fractional α,Foldable t,Functor t) => t α -> α
-- sumSq = sum . fmap (^2) 

-- varPop :: (Fractional α,Functor t,Foldable t) => t α -> α
-- -- varPop l = (/µ) . sumSq . fmap (µ-) $ l
-- varPop l = sumSq l - (µ^2)
--   where µ = mean l

-- var :: (Fractional α,Functor t,Foldable t) => t α -> α
-- var l = (/(n-1)) . sumSq . fmap (µ-) $ l
--   where n = count l
--         µ = (/n) $ sum l

-- std :: (Real α,Fractional α, Foldable t, Functor t,Floating β) => t α -> β
-- std = sqrt . fromFraction . var


median :: (Ord α,Fractional α) => [α] -> α
median l = (if r == 0
            then head
            else (µ.take 2))
           . drop (q-1) . Data.List.sort $ l
  where (q,r) = (`divMod`2) . count $ l

-- mean after applying function
-- offset to number of values (for n-1)
mean :: (Fractional α) => (α -> α) -> α -> [α] -> α
-- mean f offset values = (/ (count values + offset)) . sum . map f $ values 
mean f offset = uncurry (/) . foldr (\ v (s,c) -> (s+f v,c+1) ) (0,offset)

-- µ :: (Fractional α) => [α] -> α
-- µ = (\(a,b)->a/b) . foldl (\(s,k) e->(s+e,k+1)) (0,0)

µ :: (Fractional α) => [α] -> α
µ = mean id 0

-- computes a list's deviations from it's mean
µdeviations :: (Fractional α) => [α] -> [α]
-- µdeviations l = map (subtract mean) l
--   where mean = µ l
µdeviations = (uncurry ((.(subtract.µ)) . (flip map))) . duplicate

-- mean deviation statistic = sum |xi-µx|
mean_deviation δ = mean abs  δ . µdeviations
variance δ       = mean (^2) δ . µdeviations

squares :: (Fractional α) => [α] -> [α]
squares = map (^2)
-- sumsquares = sum . squares

ss :: (Num α,Fractional α) => [α] -> α
ss = sum . squares . µdeviations 

σ² :: Fractional c => [c] -> c
σ² = variance 0

σ²' :: Fractional c => [(c,c)] -> c
σ²' l = (sum . map (uncurry (*).applyToFst (^2)) $ l) - ((^2) . sum . map (uncurry (*)) $ l)
 
σ = sqrt . σ²

s² = variance (-1)
s = sqrt . s²

md_pop    = mean_deviation 0
md_sample = mean_deviation (-1)


-- anniversary :: Integer -> Fraction
-- anniversary k = 1 - reduce (*) [365-k+1..365] % (365^k)


-----------------------------
{- INFERENTIAL STATISTICS  -}
-----------------------------

-- unicode utf-8 utf8 characters
-- ∪∩∀∈

cov :: (Fractional α) => α -> [α] -> [α] -> α
-- cov x y = µ $ deviationsProducts x y
cov δ = ((mean id δ).) . deviationsProducts

-- population's covariance
σxy :: Fractional α => [α] -> [α] -> α
σxy = cov 0

-- sample's covariance
sxy :: Fractional α => [α] -> [α] -> α
sxy = cov (-1)


-- ∀i∈{1,n} (xi - µx) * (yi - µy)
deviationsProducts x y = zipWith (*) (µdeviations x) (µdeviations y)

-- Pearson's correlation coefficient
r :: (Fractional α,Real α) => [α] -> [α] -> Double
r x y =  ssxy / sqrt ( ss x * ss y )
  where ssxy = fromFraction $ sum $ deviationsProducts x y
        ss = fromFraction . sum . squares . µdeviations

r² :: (Fractional α,Real α) => [α] -> [α] -> Double
r² = ((^2).) . r

-- test = do
--   a <- getLine
--   b <- getLine 
--   putStrLn $ a ++ b

data AnovaTable = AnovaTable {
                             df :: [Int],
                             ssq :: [Double],
                             ms :: [Double],
                             fStat :: Double,
                             fCritical :: Double,
                             isSignificant :: Bool,
                             significance:: Int}
--                  deriving Show

instance Show AnovaTable where
  show (AnovaTable df ssq ms fStat fCritical _ significance) =
    unlines . (++[show significance,legend]). map (concatWith "\t") $ [a,b,c,d,e]
    where f s f' l = ([s]++) $ map f' l
          f' s l = f s (printf "%.4f") l 
          a = f "Df" show df
          b = f' "SS" ssq
          c = f' "MS" ms
          d = f' "F_value" [fStat] ++ [sl]
          e = f' "F_crit" [fCritical] 
          sl = [" ",".","*","**","***"] !! significance
          legend = "Signif.codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1"
--oneWay_anova :: (Real α,Fractional α,) => Double -> [[α]] -> AnovaTable
oneWay_anova α cols =
  AnovaTable {
             df = [dfb,dfw,dft],
             ssq = [ssb,ssw,sst],
             ms = [msb,msw],
             fStat = fstat,
             fCritical = fcritical,
             isSignificant = fstat > fcritical,
             significance  = significanceLevel}
  where l = map (\(Col _ v) -> v) cols
        vs = concat l
        mean = µ vs
        n = count vs
        means = map µ l
        counts = map count l
        dfb = count l - 1
        dft = count vs - 1
        dfw = dft - dfb
        ssb = sum $ zipWith (\m c -> c*(m-mean)^2 ) means counts
        ssw = sum . map ss $ l
        sst = ssb + ssw
        msb = ssb / (fromIntegral dfb)
        msw = ssw / (fromIntegral dfw)
        (Just fcritical) = f_stat_critical α dfb dfw
        (Just criticalList) =
          sequence $ map (\a -> f_stat_critical a dfb dfw) ([0.9,0.95,0.99,0.999] :: [Fraction])
        fstat = msb / msw

        significanceLevel = count . filter (fstat>) $ criticalList

test_data_anova = [[7,4,6,8,6,6,2,9],
                   [5,5,3,4,4,7,2,2],
                   [2,4,7,1,2,1,5,5]] :: [[Double]]

-- example:
-- (oneWay_anova 0.95 . dataTableFromTable 4 3 . tail . readTable) <$> readFile (path_data_stats ++ "fisher_iris.txt")


--------------
{- SAMPLING -}
--------------

-- data Sampler = Sampler {
--   randomgen :: RandomGen,
  
--         --                }
-- pick1 :: (RandomGen γ) => State γ [α] -> State γ (α,[α])
-- pick1 ( (pop,γ)) = do
--   k <- random g
--   let n = count pop
--       (hd,(t:ts)) = splitAt (k `mod` n) pop
--   return (t,hd++ts)

-- pick1 :: (RandomGen γ) => State γ [α] -> State γ (α,[α])
-- pick1 pop = do
--   k <- random
--   let (hd,(t:ts)) = splitAt (k `mod` n) pop
--   return (t,hd++ts)

-- pick1 pop g = ((t,hd++ts),g')
--   where (k,g') = random g
--         n = count pop
--         (hd,(t:ts)) = splitAt (k `mod` n) pop

-- pick One element from pop at random
pick1 :: [α] -> State StdGen (α,[α])
pick1 pop = do
  g <- get
  let (k,g') = random g
      n = count pop
      (hd,(t:ts)) = splitAt (k `mod` n) pop
  put g'
  return (t,hd++ts)


-- get a sample from pop (without replacement)
-- sample :: (Integral β,RandomGen δ) => δ -> β -> [α] -> [α]
sample 0 pop = return ([],pop)
sample k pop = do
  (e,pop') <- pick1 pop
  (l, pop'') <- sample (k-1) pop'
  return (e:l, pop'')

-- get n samples of size k from pop
samples 0 _ pop = return []
samples n k pop = do
  (s, _) <- sample k pop
  l <- samples (n-1) k pop
  return $ s : l
  


freqs :: (Foldable f , Eq α) => f α -> [(α,Int)]
freqs = quickSortBy snd . foldl' f []
  where f [] e = [(e,1)]
        f (x@(e',k):xs) e | e' == e   = ((e,k+1):xs)
                          | otherwise = x : f xs e




