-- | Experimental deterministic k-means, seeded with the first k points.
-- kmeans requires finite, non-empty points with equal dimensions. It stops
-- when every centre moves less than one unit; there is no iteration budget.
-- runTest opens feh and requires two-dimensional points.
module Kmeans where

import Data.List
import Pixels
import CSV
import Hunix
import Misc

mean l = sum l / (fromIntegral $ length l)

euclidist a b = sqrt . sum $ zipWith (\a b -> (a-b)^2 ) a b

-- helper function
onlyMin l = f l
  where minv = minimum l
        f (x : xs) | x == minv = Just x : map (const Nothing) xs
                   | otherwise = Nothing : f xs


-- assign each point to one of the clusters
clusterize centers points = foldl f (map (const []) centers) points
  where f accumul e = insert $ (`map` centers) $ euclidist e
          where insert element = zipWith g accumul . onlyMin $ element
                  where g l Nothing = l
                        g l _ = e : l


-- process kmeans with k clusters
kmeans :: Int -> [[Double]] -> [[Double]]
kmeans k l
  | k <= 0 || k > length l = error "Kmeans.kmeans: require 1 <= k <= number of points"
  | any null l || not (all ((== length (head l)) . length) l) =
      error "Kmeans.kmeans: require non-empty points of equal dimension"
  | any (any (\x -> isNaN x || isInfinite x)) l =
      error "Kmeans.kmeans: require finite coordinates"
kmeans k l = go cs (map (map (+100)) $ cs)
  where cs = take k l
        go centers olds =
          if (all (<1) $ zipWith euclidist centers olds)
          then centers
          else go newCenters centers
          where newCenters = zipWith update centers clusterized
                update old [] = old
                update _ points = map mean (transpose points)
                clusterized = clusterize centers l


---- test ----


runTest k csvFile pngFile = do
  csv <- map (map read) . parseCSV ',' <$> readFile csvFile
  let l = clusterize (kmeans k csv) csv
  pixelsToPng pngFile 80 80 l
  exec "feh" [pngFile, "--auto-zoom", "--force-aliasing"]
