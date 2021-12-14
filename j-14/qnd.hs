import Data.List(sort, foldl')

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))


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
  (firstChar, lastChar, stateZero, polymerTable) <- readInput nameFile
  putStr "Part One : "
  let stepTen = Data.List.foldl' (\acc v -> step acc polymerTable) stateZero [1..40]
  let occursTableStepTen = getOccursTable stepTen
  let foo = Map.insertWith (+) firstChar 1 (Map.insertWith (+) lastChar 1 occursTableStepTen)
  let part1 = map (`div`2) $ Map.elems occursTableStepTen
  print (maximum part1 - minimum part1)
  print part1
  --print (foldl (\a c -> a + 1) 0 (foldAlong points (head instructions)))
  --putStr "Part Two : "
  --putStrLn str

readInput nameFile = do
  content <- readFile nameFile
  let l = lines content
  let r = initTable $ head l
  let r' = initPolymers . tail . tail $ l
  return (head (head l), last (head l),r, r')

splitOn :: String -> String -> [String]
splitOn "" string = [string]
splitOn delimiters string = words (map (\c -> if c `elem` delimiters then ' ' else c) string)

initTable :: [Char] -> Map.Map (Char, Char) Int
initTable str = foldl (\acc pair -> insertIn pair 1 acc) Map.empty $ zip str (tail str)

initPolymers :: [String] -> Map.Map (Char, Char) Char
initPolymers = foldl (flip exctratPolyTuple) Map.empty

exctratPolyTuple :: [Char] -> Map.Map (Char, Char) Char -> Map.Map (Char, Char) Char
exctratPolyTuple str = Map.insert key value where
  key = (head (head splitRes), last (head splitRes))
  value = head $ last splitRes
  splitRes = splitOn "->" str

step :: Map.Map (Char, Char) Int -> Map.Map (Char, Char) Char -> Map.Map (Char, Char) Int
step m polymer = Map.foldlWithKey (\acc k a -> foldl (\acc' v -> insertIn v a acc') acc (getPolymerRes k polymer) ) Map.empty  m

insertIn :: Ord a => a -> Int -> Map.Map a Int -> Map.Map a Int
insertIn = Map.insertWith (+)

getPolymerRes (x,y) polymer = [(x, z), (z, y)] where
  z = polymer ! (x,y)

getOccursTable :: Map.Map (Char, Char) Int -> Map.Map Char Int
getOccursTable = Map.foldlWithKey (\acc (x, y) v ->  insertIn x v (insertIn y v acc)) Map.empty