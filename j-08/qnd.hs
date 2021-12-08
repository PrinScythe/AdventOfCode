

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

part1 l = 1

part2 = part1 
  