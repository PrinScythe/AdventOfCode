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
  print (amountOfIncrease content)
  putStr "Part Two : "
  print (amountOfIncrease (sumWindowed content))

readInput :: FilePath -> IO [Int]
readInput nameFile = do
  content <- readFile nameFile
  let r = read <$> lines content :: [Int]
  return r


amountOfIncrease :: (Num p, Ord t) => [t] -> p
amountOfIncrease sonarMeasure = sum (zipWith (\ prev next -> if prev < next then 1 else 0) sonarMeasure (tail sonarMeasure))

sumWindowed :: Num a => [a] -> [a]
sumWindowed sonarMeasure = zipWith3 (\x y z -> x+y+z) sonarMeasure (tail sonarMeasure) (drop 2 sonarMeasure)

