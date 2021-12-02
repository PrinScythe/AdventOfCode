main :: IO ()
main = do 
  putStrLn "--- DataSet 1 ---"
  resolve "input1.txt"
  putStrLn "\n--- DataSet 2 ---"
  resolve "input2.txt"

resolve :: FilePath -> IO ()
resolve nameFile = do
  -- content <- readInput nameFile
  putStr "Part One : "
  -- print (amountOfIncrease content)
  putStr "Part Two : "
  -- print (amountOfIncrease (sumWindowed content))