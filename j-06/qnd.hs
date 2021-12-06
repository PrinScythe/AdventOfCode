
import Data.List (partition)
import Data.Map.Lazy (Map, insertWith, empty, mapKeys, (!?), delete, insert)

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
  print (part1' content)
  putStr "Part Two : "
  print (part2' content)


readInput :: FilePath -> IO [Int]
readInput nameFile = do
  content <- readFile nameFile
  return (map read (splitOn "," content))

splitOn :: String -> String -> [String]
splitOn "" string = [string]
splitOn delimiters string = words (map (\c -> if c `elem` delimiters then ' ' else c) string)

part1 :: [Int] -> Int
part1 lanternfish = length (computeEvolution 80 lanternfish)

part2 :: [Int] -> Int
part2 lanternfish = length (computeEvolution 256 lanternfish)

computeEvolution :: Int -> [Int] -> [Int]
computeEvolution numberOfDay lanternfish = foldl inside lanternfish [1..numberOfDay]

inside :: [Int] -> Int -> [Int]
inside lanternfish day = res where
  timeProcess = map (\x -> x - 1) lanternfish
  (lanternfishParent, other) = partition (==(-1)) timeProcess
  replicated = replication lanternfishParent
  res = other ++ replicated

replication :: [Int] -> [Int]
replication = tailrec [] where
  tailrec acc [] = acc
  tailrec acc (_:tail) = tailrec (6:8:acc) tail

part1' :: [Int] -> Int
part1' lanternfishList = sum (computeEvolution' (initLanternFishMap lanternfishList) 80)

part2' :: [Int] -> Int
part2' lanternfishList = sum (computeEvolution' (initLanternFishMap lanternfishList) 256)

computeEvolution' :: Map Int Int -> Int -> Map Int Int
computeEvolution' lanternfish numberOfDay = foldl passDay lanternfish [1..numberOfDay]

passDay :: Map Int Int -> p -> Map Int Int
passDay lanternfish _ = replication' lanternfish

initLanternFishMap ::  [Int] -> Map Int Int
initLanternFishMap = foldl (\theMap key -> insertWith (+) key 1 theMap) empty

replication' :: Map Int Int -> Map Int Int
replication' lanterfishMap = res where 
  it = mapKeys (\key -> key - 1) lanterfishMap
  res = case it !? (-1) of 
    Nothing -> it
    Just value -> insertWith (+) 6 value (insert 8 value (delete (-1) it))



