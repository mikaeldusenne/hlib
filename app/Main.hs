-- import Hunix
import List

import CSV

main :: IO ()
main = do
  
  let fcells :: [[String]] -> [[String]]
      fcells = map (map (replaceStr "\"" "\\\""))
      flines :: [[String]] -> [String]
      flines = map (surround "\"" . concatWith "\", \"")
      ftable :: [String] -> String
      ftable = surround2 "[" "]" . concat . surround2 ["["] ["]"] . intercal "],\n["
  
  f <- parseCSV ',' <$> readFile "/home/mika/Downloads/SampleCSVFile_11kb.csv"
  writeFile "/home/mika/programmation/JavaScript/vue_demo_datatable/table.js" $
    ftable . flines . fcells $ f
        
-- main = putStrLn "ok"
  -- let path = "/tmp/________.test"
  -- writeFile path "hello"
  -- pth <- trim <$> exec "trash" [path]
  -- readFile pth >>= putStrLn
  -- print "-"
  -- exec "date" [] >>= putStrLn
  -- print "-"
