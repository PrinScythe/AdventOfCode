import Data.List (foldl', zip)

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
  (width, carte) <- readInput nameFile
  let depthMap = transformToDepth True width carte
  putStr "Part One : "
  print $ sum . map (\(f,s) -> f + 1) . filter (\(f, s) -> s == 0) $ zip carte depthMap
  putStr "Part Two : "
  print (part2 depthMap)

readInput :: FilePath -> IO (Int, [Int])
readInput nameFile = do
  content <- readFile nameFile
  let carte = lines content
  return (length . head $ carte, concatMap (map (\x -> read [x])) carte)

splitOn :: String -> String -> [String]
splitOn "" string = [string]
splitOn delimiters string = lines (map (\c -> if c `elem` delimiters then '\n' else c) string)

part1 :: [Int] -> Int
part1 = length . filter (==0)

part2 :: [Int] -> Int
part2 = part1

transformToDepth :: Bool -> Int -> [Int] -> [Int]
transformToDepth oobAlwaysGreater width l = snd (foldl' getDepth' (0, []) l) where
  getDepth' (pos, trans) value = (pos+1, trans ++ [getDepth oobAlwaysGreater value pos width l])

getDepth :: Bool -> Int -> Int -> Int -> [Int] -> Int
getDepth _ _ _ _ [] = error "Empty List"
getDepth True _ _ _ [_] = 0
getDepth False _ _ _ [_] = 4
getDepth oobAlwaysGreater value pos width l = length (filter (<= value) upDownLeftRight) where
    oobHeight = if oobAlwaysGreater then 10 else -1
    up = if pos - width < 0 then oobHeight else l !! (pos - width)
    down = if pos + width >= length l then oobHeight else l !! (pos + width)
    left = if (pos `mod` width) - 1 == -1 then oobHeight else l !! (pos - 1)
    right = if (pos + 1) `mod` width == 0 then oobHeight else l !! (pos + 1)
    upDownLeftRight = [up, down, left, right]


