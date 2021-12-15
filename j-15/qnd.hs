
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text.Internal.Builder as Set
import Text.XHtml.Frameset (width)
import Text.XHtml.Strict (height)

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
  (width, height, carte) <- readInput nameFile 1
  putStr "Part One : "
  res <- tailrec (Set.fromList [(0, (0,0))]) (width - 1, height - 1) carte
  print res

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

getNeighboors :: Point -> Map.Map Point Risk -> [(Risk, Point)]
getNeighboors (x, y) m = foldl (\acc (value, point) ->
  case value of
    Nothing -> acc
    Just (-1) -> acc
    Just v -> (v, point) : acc
  ) [] (map (\p -> (m Map.!? p, p)) [(x, y + 1), (x+1, y), (x-1, y), (y, x-1)])

step :: Point -> Int -> Set.Set (Risk, Point) -> Map.Map Point Risk -> Set.Set (Risk, Point)
step point dist setPoint m
  | Set.null setPoint =  Set.fromList . map (\(v, p) -> (v+dist, p)) $ getNeighboors point m
  | otherwise = foldl (\sp el@(v, p) -> Set.foldl (\sp' (v', p') -> if p /= p'
    then sp' else
      if v < v'
        then Set.insert el sp'
        else sp'
        ) sp sp ) setPoint . map (\(v, p) -> (v+dist, p)) $ getNeighboors point m

tailrec :: Set.Set (Risk, Point) -> Point -> Map.Map Point Risk -> IO Int
tailrec setPoint end m
  | snd curMin == end = return (fst curMin)
  | otherwise = do 
    print setPoint
    tailrec (step (snd curMin) (fst curMin) (Set.deleteMin setPoint) m') end m' where
    curMin = Set.findMin setPoint
    m'= update (snd curMin) m

update :: Point -> Map.Map Point Risk -> Map.Map Point Risk
update point = Map.insert point ( -1)