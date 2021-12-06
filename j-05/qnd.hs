import Data.List (intersectBy, nub)
import Data.Map.Lazy (empty, insertWith, filter, Map)

type Point = (Int, Int)
type Segment = (Point, Point)

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


readInput :: FilePath -> IO [Segment]
readInput nameFile = do
  content <- readFile nameFile
  return (map (extractSegment . splitOn "->") (lines content))

part1 :: [Segment] -> Int
part1 = computeIntersection False

part2 :: [Segment] -> Int
part2 = computeIntersection True

computeIntersection :: Bool -> [Segment] -> Int
computeIntersection includeDiagonal = length
  . Data.Map.Lazy.filter (>= 2)
  . foldl (\points point -> insertWith (+) point 1 points) empty
  . concatMap (getAllCartesianPoints includeDiagonal)

extractSegment :: [String] -> Segment
extractSegment strings = (origin, end) where
    origin = head res
    end = last res
    res = map extractPoint strings

extractPoint :: String -> Point
extractPoint string = (x, y) where
    x = read (head res)
    y = read (last res)
    res = splitOn "," string

splitOn :: String -> String -> [String]
splitOn "" string = [string]
splitOn delimiters string = words (map (\c -> if c `elem` delimiters then ' ' else c) string)

getAllCartesianPoints :: Bool -> Segment -> [Point]
getAllCartesianPoints searchDiag segment@((w, x), (y, z))
  | w == y = zip (replicate (maxOrd - minOrd + 1) w) [minOrd..maxOrd]
  | x == z = zip [minAbs..maxAbs] (replicate (maxAbs - minAbs + 1) x)
  | searchDiag = pointFromDiagonal segment
  | otherwise = [] where
    (minAbs, maxAbs) = if w < y then (w, y) else (y, w)
    (minOrd, maxOrd) = if x < z then (x, z) else (z, x)

-- trivial version
pointFromDiagonal :: Segment -> [Point]
pointFromDiagonal (start@(s1, s2),(y, z)) = if abs deltaAbs /= abs deltaOrd
  then []
  else tailrec [] start where
    deltaAbs = y - s1
    deltaOrd = z - s2
    (a, b) = (abs deltaAbs `div` deltaAbs, abs deltaOrd `div` deltaOrd)
    tailrec acc current@(w, x)
      | w == y && x == z = acc ++ [current]
      | otherwise = tailrec (acc ++ [current]) (w + a, x + b)
