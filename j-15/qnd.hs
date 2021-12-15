
import Data.List (zip, sortOn)
import qualified Data.Maybe as Maybe
import qualified Data.Map.Strict as Map
import Data.Ord ( Down(Down) )
import qualified Data.Maybe as Maybe

type Point = (Int, Int)
type Height = Int 
type Width = Int
type Risk = Int


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
  (width, height, carte) <- readInput nameFile 5
  putStr "Part One : "
  print (width, height, carte)

createPoints :: Int -> Int -> [Point]
createPoints width height = [(x, y) | x <- [0..(height -1)], y <- [0..(width - 1)]]

readInput :: FilePath -> Int -> IO (Width, Height, Map.Map Point Risk)
readInput nameFile factor = do
  content <- readFile nameFile
  let carteStr = lines content
  let w = (length . head $ carteStr) * factor
  let h = length carteStr * factor
  return (w, h, Map.fromList . zip (createPoints w h) $ concatMap (concat . replicate factor . map (\x -> read [x])) (concat . replicate factor $ carteStr))

splitOn :: String -> [Char] -> [String]
splitOn "" string = [string]
splitOn delimiters string = lines (map (\c -> if c `elem` delimiters then '\n' else c) string)

getNeighboors (x, y) m = foldl (\acc (value, point) -> 
  case value of
    Nothing -> acc
    Just v -> (v, point) : acc
  ) [] [((m Map.!? ((x - 1), y)), ((x - 1), y))), ((m Map.!? ((x + 1), y), (m Map.!? (x, (y - 1)), (x, (y - 1))), ((m Map.!? (x, (y - 1)), y)]
