{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main (main) where

import Control.Exception (ErrorCall, evaluate, try)
import Control.Monad (unless)
import Control.Lens ((&), (.~), (?~))
import qualified Codec.Xlsx as Xlsx
import qualified Data.Aeson as Aeson
import Data.Default.Class (def)
import qualified Data.List as DL
import qualified Data.Map.Strict as Map
import qualified Network.HTTP.Conduit as HTTP
import System.Exit (die)
import System.Timeout (timeout)
import Test.QuickCheck

import qualified Bases
import qualified CSV
import qualified DF
import qualified Html
import qualified Json
import qualified Kmeans
import qualified List
import qualified Maths
import qualified Matrix
import qualified Requester
import qualified Stats
import qualified Trees

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual name expected actual = do
  result <- timeout 2000000 (evaluate (expected == actual))
  case result of
    Just True -> putStrLn ("PASS " ++ name)
    Just False -> die (name ++ ": expected " ++ show expected ++ ", got " ++ show actual)
    Nothing -> die (name ++ ": timed out")

assertError :: String -> Int -> IO ()
assertError name value = do
  result <- timeout 2000000 (try (evaluate value) :: IO (Either ErrorCall Int))
  case result of
    Just (Left _) -> putStrLn ("PASS " ++ name)
    _ -> die (name ++ ": expected a prompt ErrorCall")

propertyCheck :: Testable prop => String -> prop -> IO ()
propertyCheck name prop = do
  putStrLn name
  result <- quickCheckWithResult stdArgs {maxSuccess = 200} (within 2000000 prop)
  unless (isSuccess result) (die (name ++ ": property failed"))

main :: IO ()
main = do
  assertEqual "safe_nth empty" Nothing (List.safe_nth (2 :: Int) ([] :: [Int]))
  assertEqual "safe_nth past end" Nothing (List.safe_nth (3 :: Int) [10, 20 :: Int])
  assertEqual "safe_nth is one-based" (Just 20) (List.safe_nth (2 :: Int) [10, 20 :: Int])
  assertEqual "safe_nth rejects zero" Nothing (List.safe_nth (0 :: Int) [10 :: Int])
  assertEqual "reduce singleton" 7 (List.reduce (+) [7 :: Int])
  assertEqual "flatten empty" ([] :: [Int]) (List.flatten [])
  assertEqual "flatten singleton" [1 :: Int] (List.flatten [[1]])
  assertEqual "transpose empty" ([] :: [[Int]]) (List.transpose [])
  assertEqual "transpose ragged" [[1, 2], [3 :: Int]] (List.transpose [[1], [2, 3]])
  assertEqual "empty replacement pattern" "abc" (List.replaceStr "" "x" "abc")
  assertError "non-positive chunk size" (length (List.splitEach 0 [1 :: Int]))
  assertError "empty separator" (length (List.splitWhen "" "abc"))
  assertEqual "median odd" 2 (Stats.median [3, 1, 2 :: Double])
  assertEqual "median even" 2.5 (Stats.median [4, 2, 1, 3 :: Double])
  assertEqual "median singleton" 7 (Stats.median [7 :: Double])
  assertError "median empty" (round (Stats.median ([] :: [Double])))
  assertEqual "choose edges" [1, 1, 5, 10, 1, 0, 0]
    [Stats.choose 0 0, Stats.choose 5 0, Stats.choose 5 1, Stats.choose 5 2,
     Stats.choose 5 5, Stats.choose 5 6, Stats.choose 5 (-1) :: Integer]
  assertEqual "binomial mass" (1 :: Rational) (sum (Stats.dbinom (6 :: Integer) (1/3)))
  assertEqual "population variance" (2/3 :: Rational) (Stats.σ² [1,2,3])
  assertEqual "sample variance" (1 :: Double) (Stats.s² [1,2,3])
  assertEqual "rectangular matrix row" [4,5,6 :: Int]
    (Matrix.rowN 2 (Matrix.Matrix 2 3 [1..6]))
  assertEqual "matrix columns" [[1,4], [2,5], [3,6 :: Int]]
    (Matrix.cols (Matrix.Matrix 2 3 [1..6]))
  assertEqual "CSV quoted cells and CRLF"
    [["a,b", "say \"hi\"", ""], ["x\ny", "z", "q"]]
    (CSV.parseCSV ',' "\"a,b\",\"say \"\"hi\"\"\",\r\n\"x\ny\",z,q\r\n")
  assertEqual "CSV preserves interior empty cells" [["a","","b"]]
    (CSV.parseCSV ',' "a,,b")
  assertEqual "CSV empty input" [] (CSV.parseCSV ',' "")
  let sheet = (def :: Xlsx.Worksheet) & Xlsx.wsCells .~ Map.fromList
        [ ((2,3), def & Xlsx.cellValue ?~ Xlsx.CellText "name")
        , ((2,4), def & Xlsx.cellValue ?~ Xlsx.CellText "score")
        , ((3,3), def & Xlsx.cellValue ?~ Xlsx.CellText "Alice")
        , ((3,4), def & Xlsx.cellValue ?~ Xlsx.CellDouble 10)
        ]
  assertEqual "XLSX range keeps Int coordinates" ((2,3),(3,4)) (DF.wsRange sheet)
  assertEqual "XLSX offset sheet titles" ["name","score"] (DF.df'titles (DF.ws'to'DF sheet))
  assertEqual "XLSX offset sheet values" [["Alice","10.0"]] (DF.df'data (DF.ws'to'DF sheet))
  assertEqual "fraction arithmetic" (Maths.readFraction "2")
    (Maths.readFraction "1.25" + Maths.readFraction "3/4")
  let object = Aeson.object ["name" Aeson..= ("hlib" :: String)]
  assertEqual "Aeson 2 member" (Just (Aeson.String "hlib")) (Json.lookupMaybe "name" object)
  assertEqual "missing JSON member" Nothing (Json.lookupMaybe "missing" object)
  assertEqual "non-object JSON" Nothing (Json.lookupMaybe "name" Aeson.Null)
  assertEqual "legacy JSON lookup" "hlib" (Json.unString (Json.lookup "name" object))
  assertEqual "safe JSON string" Nothing (Json.unStringMaybe Aeson.Null)
  get <- Requester.build Requester.GET "https://example.com/path" (HTTP.RequestBodyBS "ignored")
  post <- Requester.build Requester.POST "https://example.com/path" (HTTP.RequestBodyBS "payload")
  assertEqual "GET method" "GET" (HTTP.method get)
  assertEqual "GET path" "/path" (HTTP.path get)
  assertEqual "POST method" "POST" (HTTP.method post)
  case HTTP.requestBody post of
    HTTP.RequestBodyBS body -> assertEqual "POST body" "payload" body
    _ -> die "POST body changed representation"
  assertEqual "HTML text escaping" "&lt;script&gt;&amp;&quot;&#39;" (show (Html.Text "<script>&\"'"))
  assertEqual "HTML attribute escaping" "<span class=\"\" title=\"a&amp;&quot;b\"> </span>"
    (show (Html.E "span" [] (Map.singleton "title" "a&\"b") []))
  assertEqual "kmeans waits for all centres" [[5], [30]] (Kmeans.kmeans 2 [[0], [10], [20], [30], [40]])
  assertEqual "kmeans retains empty cluster centre" [[0], [0]] (Kmeans.kmeans 2 [[0], [0]])
  assertError "kmeans rejects ragged points" (length (Kmeans.kmeans 1 [[1], [2,3]]))
  assertError "kmeans rejects zero clusters" (length (Kmeans.kmeans 0 [[1]]))
  assertEqual "tree traversal order" [1,2,3 :: Int]
    (Trees.toList (Trees.Node 1 [Trees.Leaf 2, Trees.Leaf 3]))
  propertyCheck "chunking preserves input" $ \(Positive n) (xs :: [Int]) ->
    concat (List.splitEach n xs) == xs
  propertyCheck "mergeSort agrees with Data.List.sort" $ \(xs :: [Int]) ->
    List.mergeSort xs == DL.sort xs
  propertyCheck "choose row sums" $ \(NonNegative n0) ->
    let n = toInteger (n0 `mod` (30 :: Int))
    in sum [Stats.choose n k | k <- [0..n]] == 2 ^ n
  propertyCheck "base-16 round trip" $ \(NonNegative n0) ->
    let n = n0 `mod` (1000000 :: Int)
    in Bases.from'base 16 (Bases.to'base 16 n) == n
