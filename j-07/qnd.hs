
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
  putStr "Part Tw' : "
  print (computeFuelBy (\n -> (n * (n+1)) `div` 2) (part2' content) content)


readInput :: FilePath -> IO [Int]
readInput nameFile = do
  content <- readFile nameFile
  return (map read (splitOn "," content))

splitOn :: String -> String -> [String]
splitOn "" string = [string]
splitOn delimiters string = words (map (\c -> if c `elem` delimiters then ' ' else c) string)

part1 :: [Int] -> Int
part1 l = foldl (\acc cur -> acc + abs (cur - mediane)) 0  l where
  mediane = sort l !! (length l `div` 2 )


part2 :: [Int] -> Int
part2 l' = minimum l where
  computeFuel point = foldl (\a x -> a + sumNumber (abs (x - point))) 0
  sumNumber n = (n * (n+1)) `div` 2
  l = map (`computeFuel` l') [minimum l'..maximum l']

part2' :: [Int] -> Int
part2' l = res where
  u = sum l
  p = l !! (u `mod` length l)
  x = u `div` length l
  res = if p > x then x + 1 else x

computeFuelBy :: (Int -> Int) -> Int -> [Int] -> Int
computeFuelBy f point = foldl (\acc cur -> acc + f (abs(cur - point))) 0
  