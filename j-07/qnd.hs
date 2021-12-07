
import Data.List (sort, foldl1)

main :: IO ()
main = do
  putStrLn "--- Example ---"
  resolve "example.txt"
  putStrLn "--- DataSet 1 ---"
  resolve "input1.txt"
  putStrLn "--- DataSet 2 ---"
  resolve "input2.txt"

resolve :: FilePath -> IO ()
resolve nameFile = do
  content <- readInput nameFile
  putStr "Part One : "
  print (part1 content)
  putStr "Part Two : "
  print (part2 content)


readInput :: FilePath -> IO [Int]
readInput nameFile = do
  content <- readFile nameFile
  return (map read (splitOn "," content))

splitOn :: String -> String -> [String]
splitOn "" string = [string]
splitOn delimiters string = words (map (\c -> if c `elem` delimiters then ' ' else c) string)

part1 :: [Int] -> Int
part1 l = foldl (\acc cur -> acc + abs (cur - ecartType)) 0  l where
  ecartType = sort l !! (length l `div` 2 )


part2 :: [Int] -> Int
part2 l' = minimum l where
  foo b = foldl (\a x -> a + sumNumber (abs (x - b))) 0
  sumNumber n = (n * (n+1)) `div` 2
  ecartType l = sort l !! (length l `div` 2 )
  l = map (`foo` l') [minimum l'..maximum l']
  