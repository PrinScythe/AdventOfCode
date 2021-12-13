import Data.List(sort)

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

type Point = (Int, Int)
type Instructions = [(Int, Int)]

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
  (points, instructions) <- readInput nameFile
  putStr "Part One : "
  print (foldl (\a c -> a + 1) 0 (foldAlong points (head instructions)))
  putStr "Part Two : "
  let fa = foldAll points instructions
  let str = mapToStr . rotatemap $ bar fa
  putStrLn str

readInput :: FilePath -> IO (Set.Set Point, Instructions)
readInput nameFile = do
  content <- readFile nameFile
  let l = lines content
  let r = foldl foo (Set.empty, []) l
  return r

splitOn :: String -> String -> [String]
splitOn "" string = [string]
splitOn delimiters string = lines (map (\c -> if c `elem` delimiters then '\n' else c) string)

foldAlong :: Set.Set Point -> Point -> Set.Set Point
foldAlong points (x, 0) =  res where
  (stayed, folded) = Set.partition (\(x', _) ->  x' <= x) points
  stayedFilered = Set.filter (\(a, _) -> a < x) stayed
  res = foldl (\acc (a, b) -> Set.insert (a - ((a - x) * 2), b) acc) stayedFilered folded
foldAlong points (0, y) =  res where
  (stayed, folded) = Set.partition (\(_, y') ->  y' <= y) points
  stayedFilered = Set.filter (\(_, b) -> b < y) stayed
  res = foldl (\acc (b, a) -> Set.insert (b, a - ((a - y) * 2)) acc) stayedFilered folded
foldAlong _ _ = error "Diantre"

foldAll :: Set.Set Point -> Instructions -> Set.Set Point
foldAll = foldl foldAlong


initPoints :: Int -> Int -> Map.Map Point String
initPoints width height = Map.fromList $ zip [(x, y) | x <- [0..height], y <- [0..width]] [" " | x <- [0..height], y <- [0..width]]

foo :: (Set.Set Point, Instructions) -> String -> (Set.Set Point, Instructions)
foo acc "" = acc
foo acc@(points, instructions) line = case splitOn "," line of
  [x, y] -> (Set.insert (read x, read y) points, instructions)
  _ -> case splitOn "=" line of
    ["fold along x", x] -> (points, instructions ++ [(read x, 0)])
    ["fold along y", y] -> (points, instructions ++ [(0, read y)])
    _ -> acc

bar :: Set.Set Point -> Map.Map Point String
bar = Set.foldl (\acc p -> Map.insert p "█" acc) (initPoints 7 40)


test :: Ord a => a -> Set.Set a
test p = Set.insert p (Set.insert p Set.empty)

mapToStr ::  Map.Map Point [Char] -> [Char]
mapToStr = Map.foldlWithKey (\acc (_, y) v -> if y == 0 then acc ++ "\n" ++ v else acc ++ v) ""

rotatemap :: (Ord a1, Ord b) => Map.Map (b, a1) a2 -> Map.Map (a1, b) a2
rotatemap = Map.mapKeys (\(x, y) -> (y, x))

