main :: IO ()
main = do 
  putStrLn "--- DataSet 1 ---"
  resolve "input1.txt"
  putStrLn "\n--- DataSet 2 ---"
  resolve "input2.txt"

resolve :: FilePath -> IO ()
resolve nameFile = do
  content <- readInput nameFile
  putStr "Part One : "
  print (computeProductOfLastPosition content)
  putStr "Part Two : "
  print (computeProductOfLastPosition' content)

readInput :: FilePath -> IO [String]
readInput nameFile = do
  content <- readFile nameFile
  return (lines content)

newPosFrom :: (Num a, Read a) => [a] -> String -> [a]
newPosFrom prev direction = case words direction of
    ["forward", t] -> [head prev, last prev + read t]
    ["down", t] -> [head prev + read t, last prev]
    ["up", t] -> [head prev - read t, last prev]
    _ -> [0,0]


newPosAimFrom :: (Num a, Read a) => [a] -> String -> [a]
newPosAimFrom prevPosAim direction = case words direction of
    ["forward", t] -> [head prevPosAim + last prevPosAim * read t, head (tail prevPosAim) + read t, last prevPosAim]
    ["down", t] -> [head prevPosAim, head (tail prevPosAim), last prevPosAim + read t]
    ["up", t] -> [head prevPosAim, head (tail prevPosAim), last prevPosAim - read t]
    _ -> prevPosAim

computeProductOfLastPosition :: (Num a, Foldable t, Read a) => t String -> a
computeProductOfLastPosition directions = product (foldl newPosFrom [0,0] directions)

computeProductOfLastPosition' :: (Foldable t, Num a, Read a) => t String -> a
computeProductOfLastPosition' directions = head l * head (tail l) where l = foldl newPosAimFrom [0, 0, 0] directions

