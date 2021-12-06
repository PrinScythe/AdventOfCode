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
  print content
  --putStrLn "Part One : "
  --c <- part1 tirage rawLines
  --putStrLn "Part Two : "
  --c2 <- part2 tirage rawLines
  --print "the end"


readInput :: FilePath -> IO [Segment]
readInput nameFile = do
  content <- readFile nameFile
  return (map (extractSegment . splitOn "->") (lines content))

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

getAllCartesianPoints ((w, x), (y, z)) 
  | w == y = zip (replicate (abs (z - x)) w) [minOrd..maxOrd]
  | x == z = zip (replicate (abs (y - w)) x) [minAbs..maxAbs]
  | otherwise = [] where 
    (minAbs, maxAbs) = if w < y then (w, y) else (y, w)
    (minOrd, maxOrd) = if x < z then (x, z) else (z, x)
