
import Data.List (zip, sortOn)
import qualified Data.Maybe
import qualified Data.Map.Strict as Map
import Data.Ord ( Down(Down) )

type Point = (Int, Int)
type Depth = Int
type Value = Int


main :: IO ()
main = do
  putStrLn "--- Example ---"
  resolve "example.txt"
  --putStrLn "--- DataSet 1 ---"
  --resolve "input1.txt"
  --putStrLn "--- DataSet 2 ---"
  --resolve "input2.txt"


resolve :: FilePath -> IO ()
resolve nameFile = do
  (width, carte) <- readInput nameFile
  let height = length carte `div` width
  let carteMap = Map.fromList (zip (createPoints width height) carte)
  let depthMap = transformToDepth True width height carteMap
  putStr "Part One : "
  print $ part1 depthMap
  putStr "Part Two : "
  print $ depthMap
  print $ whatAmIDoing depthMap
  print $ isThisThingWorking depthMap

createPoints :: Int -> Int -> [Point]
createPoints width height = [(x, y) | x <- [0..(height -1)], y <- [0..(width - 1)]]

readInput :: FilePath -> IO (Int, [Value])
readInput nameFile = do
  content <- readFile nameFile
  let carte = lines content
  return (length . head $ carte, concatMap (map (\x -> read [x])) carte)

splitOn :: String -> [Char] -> [String]
splitOn "" string = [string]
splitOn delimiters string = lines (map (\c -> if c `elem` delimiters then '\n' else c) string)

part1 :: Map.Map Point (Value, Depth) -> Int
part1 = Map.foldl (\acc (value, depth) -> if depth == 0 then value + 1 + acc else acc ) 0

transformToDepth :: Bool
  -> Int
  -> Int
  -> Map.Map Point Value
  -> Map.Map Point (Value, Depth)
transformToDepth oobAlwaysGreater width height m = Map.mapWithKey (\k a -> getDepth oobAlwaysGreater a k width height m) m

getDepth :: Bool -> Value -> Point -> Int -> Int -> Map.Map Point Value -> (Value, Depth)
getDepth oobAlwaysGreater value pos@(x, y) width height m = (value, length (Prelude.filter (<= value) upDownLeftRight))  where
    oobHeight = if oobAlwaysGreater then 9 else -1
    up = Data.Maybe.fromMaybe oobHeight (m Map.!? (x - 1, y))
    down = Data.Maybe.fromMaybe oobHeight (m Map.!? (x + 1, y))
    left = Data.Maybe.fromMaybe oobHeight (m Map.!? (x, y - 1))
    right = Data.Maybe.fromMaybe oobHeight (m Map.!? (x, y + 1))
    upDownLeftRight = [up, down, left, right]


whatAmIDoing :: Map.Map Point (Value, Depth) -> Map.Map Int [Point]
whatAmIDoing m = basins where
  (_, basins, _) = Map.foldlWithKey innerFun (Map.empty, Map.empty, 0) m
  innerFun acc _ (_, 4) = acc
  innerFun (taPointBasin, taBasinPoint, nextBasin) point@(x, y) (_, _) =
    case taPointBasin Map.!? (x - 1, y) of
      Just basinNumber -> (Map.insert point basinNumber taPointBasin, Map.insertWith (++) basinNumber [point] taBasinPoint, nextBasin)
      Nothing -> case taPointBasin Map.!? (x, y - 1) of
        Just basinNumber -> (Map.insert point basinNumber taPointBasin, Map.insertWith (++)  basinNumber [point] taBasinPoint, nextBasin)
        Nothing -> (Map.insert point nextBasin taPointBasin, Map.insert nextBasin [point] taBasinPoint, nextBasin + 1)

isThisThingWorking :: Map.Map Point (Value, Depth) -> Int
isThisThingWorking m = product . take 3 . sortOn Down . map length . Map.elems $ whatAmIDoing m