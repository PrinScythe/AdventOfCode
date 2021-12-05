import Data.List (zipWith5, find)
import Text.XHtml (content)

type Grid = [(Int, Int, LineOrColumn)]
type RawGrid = [LineOrColumn]
type LineOrColumn = [Int]

main :: IO ()
main = do
  --putStrLn "--- Example ---"
  --resolve "example.txt"
  putStrLn "--- DataSet 1 ---"
  resolve "input1.txt"
  --putStrLn "\n--- DataSet 2 ---"
  --resolve "input2.txt"

resolve :: FilePath -> IO ()
resolve nameFile = do
  content <- readInput nameFile
  let tirage = map read (splitOn ',' (head content)) :: [Int]
  let rawLines = filter (/="") (tail content)
  putStrLn "Part One : "
  c <- (part1 tirage rawLines)
  print c
  --putStr "Part Two : "
  --print (part2 content)

readInput :: FilePath -> IO [String]
readInput nameFile = do
  content <- readFile nameFile
  return (lines content)

splitGrid :: Int -> [String] -> [[String]]
splitGrid gridSize l = tailRec [] l where
  tailRec acc [] = acc
  tailRec acc l  = tailRec ( take gridSize l : acc) (drop gridSize l)

initInfos :: RawGrid -> Grid
initInfos = map (\lineOrColumn -> (0, sum lineOrColumn, lineOrColumn))

flatToGrid5 :: [String] -> RawGrid
flatToGrid5 l =  line ++ column where
  line = map (map read . words) l
  column = rotate5 line

rotate5 :: [LineOrColumn] -> [LineOrColumn]
rotate5 [a, b, c, d, e] = zipWith5 (\v w x y z -> [v, w, x, y, z]) a b c d e
rotate5 _ = error "Please don't be silly"

searchInGrid :: Int -> Int -> Grid -> Grid
searchInGrid gridSize number grid = checkGrid (map searchInLine grid) where
  searchInLine l@(count, sum, lc) = case find (== number) lc of
    Nothing -> l
    Just _ -> (count + 1, sum - number, lc)
  checkGrid grid = case find ((== gridSize) . getCountFormLine) grid of
    Nothing -> grid
    Just x0 -> error ("Win With : " ++ show (computeVictory gridSize number grid))

getCountFormLine :: (Int, Int, LineOrColumn) -> Int
getCountFormLine (count, _, _) = count

getSumFormLine :: (Int, Int, LineOrColumn) -> Int
getSumFormLine (_, sum, _) = sum

computeVictory :: Int -> Int -> Grid -> Int
computeVictory gridSize number grid = number * sum (map getSumFormLine (take gridSize grid))

splitOn :: Char -> String -> [String]
splitOn delimiter string = words (map (\c -> if c == delimiter then ' ' else c) string)

part1 tirage rawLines = foldl (\allGrids number -> do
    all <- allGrids
    print all
    return (map (searchInGrid 5 number) all)
  ) gridsInit tirage where
  rawLinesGroupByGrid = splitGrid 5 rawLines
  gridsInit =  do return (reverse (map (initInfos . flatToGrid5) rawLinesGroupByGrid))

