module Main (main) where

import qualified CSV
import qualified List
import qualified Maths
import qualified Stats

main :: IO ()
main = do
  print (CSV.parseCSV ',' "name,score\nAlice,10\nBob,14\n")
  print (List.safe_nth 2 [10, 14 :: Int])
  print (Stats.median [10, 14 :: Double])
  print (Stats.choose 5 2 :: Integer)
  print (Maths.readFraction "1.25" + Maths.readFraction "3/4")
