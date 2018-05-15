module Matrix where

import Maths
-- import Fraction
import List
import Tuple
-- import Parser

type Cell = Fraction
type Row α = [α]
data Col α = Col { title :: String,
                   values :: [α] }
             deriving Show
-- type Matrix = [Row]

data Matrix α = Matrix {
  nRows :: Int,
  nCols :: Int,
  cells :: [α]}  -- TODO change to [[]]
  
toMatrix :: [[String]] -> Matrix (Col String)
toMatrix cells = Matrix r c . map (uncurry Col . headNtail) . transpose $ cells
  where r = count cells
        c = count . head $ cells
  

splitRows = lines
splitCols = words

unrows = unlines
-- unclos = reduce (\ a b -> a ++ ('\t' : b))
uncols = reduce ((.('\t':)) . (++))

readRow :: String -> [Cell]
readRow = map read . splitCols

instance (Show α) => Show (Matrix α) where
  show =  surround . unrows . map (uncols . map show) . rows
    where top =  ( replicate 40 '-') 
          surround a = top ++ ('\n':a) ++ top

readMatrix :: String -> Matrix Cell
readMatrix s = if ok
               then Matrix m n cells
               else error "error: Matrix: no parse"
  where l = splitRows s
        cells = flatten
                . map readRow
                $ l
        m = fromIntegral . count $ l
        (n,r) = (`divMod`m) . fromIntegral . count $ cells
        ok = r == 0

rowN :: Int -> Matrix α -> [α]
rowN k (Matrix m n cells) = take m . drop ((k-1)*m) $ cells 

rows :: Matrix α -> [[α]]
rows (Matrix m n cells) = splitEach n cells

cols = transpose . rows



showTable :: [[String]] -> String
showTable = showTableWithHeaders False

instance Functor Matrix where
  fmap f (Matrix m n cells) = Matrix m n $ fmap f cells

-- instance Applicative Matrix where
--   pure :: 

-- instance (Num α) => Num (Matrix α) where
--   fromInteger = Matrix 1 1 . (:[]) . fromInteger
--   (+) = elementwise (+)
--   signum = fmap signum
--   (*) = mulMatrices
--   abs = fmap abs
--   negate = fmap negate

elementwise :: (a -> b -> c) -> (Matrix a -> Matrix b -> Matrix c)
elementwise f ma@(Matrix m n cellsa) mb@(Matrix o p cellsb) =
    Matrix m n . flatten $ zipWith (zipWith f) (rows ma) (rows mb)

-- mulMatrices :: Num α => Matrix α -> Matrix α -> Matrix α
-- mulMatrices ma@(Matrix m n ca) mb@(Matrix o p cb) =
--   Matrix m p . map (reduce (+)) $ zipWith (*) <$> rows ma <*> cols mb

scaleMatrix k = fmap (*k)


-- quicksort on Matrix' rows
-- with function Row -> Ord
-- sortMatrixRows :: (Eq α,Ord α,Num α,Ord a) => (Row α -> a) -> Matrix α -> Matrix α
-- sortMatrixRows f matrix@(Matrix m n c) =
--   Matrix m n . flatten . quickSortBy f . rows $ matrix

-- pivotPosition :: (Eq α,Num α) => Row α -> Int
-- pivotPosition  =  count . takeWhile (==0)

-- pivot :: (Eq α,Num α) => Row α -> α
-- pivot = head . dropWhile (==0)

-- simplifyPivot :: Row Fraction -> Row Fraction
-- simplifyPivot r = map (/ pivot r) r

-- addRows :: (Num α) => Row α -> Row α -> Row α
-- addRows = zipWith (+)
-- mulRows = zipWith (*)


-- solveMatrix ::  Matrix Fraction -> Matrix Fraction
-- solveMatrix matrx@(Matrix m n c) =
--    Matrix m n
--    . flatten
--    . reverse
--    . (\ a -> solve [] (head a) (tail a))
--    . rows
--    . sortMatrixRows pivotPosition
--    $ matrx

--   where solve acc1 [] [] = acc1
--         solve acc1 r [] = (r':acc1)
--           where  r' = if (all (==0)) r
--                      then r
--                      else simplifyPivot r
--         solve acc1 r acc2
--           | all (==0) r = solve (r:acc1)  (head acc2) (tail acc2)
--           | otherwise = solve
--                         (r':acc1')  (head acc2') (tail acc2')
--                         -- r''
--                         -- acc2''
--           where r' = if (all (==0)) r
--                      then r
--                      else simplifyPivot r
--                 --acc1' = acc1
--                 --acc2' = map solve' acc2
--                 (acc1',acc2') = applyToTuple (map solve') (acc1,acc2)
--                   where solve' l = addRows l
--                                    . (`map`r')
--                                    . (*) . negate
--                                    . (!! pivotPosition r')
--                                    $ l

-----------------------------------------------
                -- (r'',acc2'') = if count acc2' > 0
                --       then (head acc2',tail acc2')
                --       else ([],[])
--                acc2'' = if count acc2' > 0
-- reduceRowFromPivot :: Row Fraction -> Row Fraction -> Row Fraction
-- reduceRowFromPivot piv r = addRows l
--                                    . (`map`piv)
--                                    . (*) . negate
--                                    . (!! pivotPosition piv)
--                                    . transpose
--                                    $ l


-- solveMatrix ::  Matrix Fraction -> Matrix Fraction
-- solveMatrix matrix@(Matrix m n c) =
--    Matrix m n
--    . flatten
--    . solve [] []
--    . rows
--    . sortMatrixRows pivotPosition
--    $ matrix

--   where solve acc1 [] [] = acc1
--         solve acc1 r acc2 = solve
--                             (r':acc1')
--                             (head acc2')
--                             (tail acc2')
--           where r' = simplifyPivot r
--                 solve' [] = []
--                 solve' l = zipWith addRows
--                          . map (r'*)
--                          . map negate
--                          . (!! pivotPosition r')
--                          . transpose
--                          $ l
--                 (acc1',acc2') = applyToTuple solve' (acc1,acc2)



loadMatrix n = readMatrix <$> readFile path
  where path = "matrix/matrix"++(show n)++".data"

