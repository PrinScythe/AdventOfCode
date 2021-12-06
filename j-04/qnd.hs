import Data.List (zipWith5, find, partition)

type Grid = [(Int, Int, LineOrColumn)]
type RawGrid = [LineOrColumn]
type LineOrColumn = [Int]

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
  let tirage = map read (splitOn ',' (head content)) :: [Int]
  let rawLines = filter (/="") (tail content)
  putStrLn "Part One : "
  c <- part1 tirage rawLines
  putStrLn "Part Two : "
  c2 <- part2 tirage rawLines
  print "the end"

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
searchInGrid gridSize number = map searchInLine where
  searchInLine l@(count, sum, lc) = case find (== number) lc of
    Nothing -> l
    Just _ -> (count + 1, sum - number, lc)

isWinning :: Int -> Grid -> Bool
isWinning gridSize grid = case find ((== gridSize) . getCountFormLine) grid of
  Nothing -> False
  Just _ -> True

getCountFormLine :: (Int, Int, LineOrColumn) -> Int
getCountFormLine (count, _, _) = count

getSumFormLine :: (Int, Int, LineOrColumn) -> Int
getSumFormLine (_, sum, _) = sum

computeVictory :: Int -> Int -> Grid -> Int
computeVictory gridSize number grid = number * sum (map getSumFormLine (take gridSize grid))

splitOn :: Char -> String -> [String]
splitOn delimiter string = words (map (\c -> if c == delimiter then ' ' else c) string)

part1 :: [Int] -> [String] -> IO [Grid]
part1 = foundNthWinner 1

part2 :: [Int] -> [String] -> IO [Grid]
part2 tirage rawlines = foundNthWinner (length rawlines `div` 5) tirage rawlines

foundNthWinner :: Int -> [Int] -> [String] -> IO [Grid]
foundNthWinner nthWinner tirage rawLines = foldl (\allGrids number -> do
    all <- allGrids
    let gridsUpdated = map (searchInGrid 5 number) all
    let (winning, loosing) = partition (isWinning 5) gridsUpdated
    init <- gridsInit
    let amountLoosing = length loosing
    let amountInit = length init
    let amountWinning = length winning
    let delta = amountInit - (amountWinning + amountLoosing)
    if amountLoosing == length init - nthWinner && not (null winning)
      then print (show nthWinner ++ "nt answer is " ++ show (computeVictory 5 number (winning !! (nthWinner - delta - 1))))
      else putStr ""
    return loosing
  ) gridsInit tirage where
  rawLinesGroupByGrid = splitGrid 5 rawLines
  gridsInit = return (reverse (map (initInfos . flatToGrid5) rawLinesGroupByGrid))

