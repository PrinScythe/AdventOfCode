
import qualified Data.Map.Lazy as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

type Area = (Range, Range, Range)
type Range = (Int, Int)
type Step = (Action, Area)
type Action = Int
type Point = (Int, Int, Int)
type Cuboid = Map.Map Point Action


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
  steps <- readInput nameFile
  putStr " -- Part 1 > "
  print $ fst $ processSteps (extractStep "off x=-50..50,y=-50..50,z=-50..50") (extractArea "x=-50..50,y=-50..50,z=-50..50") steps

readInput :: FilePath -> IO  [Step]
readInput nfile = do
  content <- readFile nfile
  return (map extractStep . lines $ content)

splitOn :: [Char] -> String -> [String]
splitOn "" string = [string]
splitOn delimiters string = filter (not . null) $ lines (map (\c -> if c `elem` delimiters then '\n' else c) string)

toTuple :: (a -> b) -> [a] -> (b, b)
toTuple f = toTuple' f f

toTuple' :: (a -> b) -> (a -> c) -> [a] -> (b, c)
toTuple' f f' l = (f $ head l, f' $ last l)

toTrouple :: (a -> c) -> [a] -> (c, c, c)
toTrouple f l = (f $ head l, f . head $ tail l, f $ last l)

extractArea :: String -> Area
extractArea = toArea . map ((toRange . splitOn ".") . last . splitOn "=") . splitOn "," where
  toRange = toTuple read
  toArea = toTrouple id

stringToAction :: String -> Action
stringToAction "on" = 1
stringToAction "off" = 0
stringToAction _ = error "Not an Action"

extractStep :: String -> (Action, Area)
extractStep = toTuple' stringToAction extractArea . splitOn " "

processStep :: Step -> Area -> (Int, Cuboid) -> (Int, Cuboid)
processStep (action, ((x1, x2), (y1, y2), (z1, z2))) ((mx1, mx2), (my1, my2), (mz1, mz2)) (numberOn, m)
 | null m = ((x2 - x1 + 1) * (y2 - y1 + 1) * (z2 - z1 + 1) * action, Map.fromList [((x, y, z), action) | x <- [(maximum [mx1, x1])..(minimum [mx2, x2])], y<-[(maximum [my1, y1])..(minimum [my2, y2])], z<-[(maximum [mz1, z1])..(minimum [mz2, z2])]])
 | otherwise = Map.foldlWithKey foldOp (numberOn, m) (Map.fromList [((x, y, z), action) | x <- [(maximum [mx1, x1])..(minimum [mx2, x2])], y<-[(maximum [my1, y1])..(minimum [my2, y2])], z<-[(maximum [mz1, z1])..(minimum [mz2, z2])]]) where
    foldOp acc@(n, space) p action = case space Map.!? p of
      Nothing -> acc
      Just a
        | a == action -> acc
        | action == 0 -> (n - 1, Map.insert p action space)
        | otherwise -> (n + 1, Map.insert p action space)

processSteps :: Step -> Area -> [Step] -> (Int, Cuboid)
processSteps firstStep area = foldl (\acc step -> processStep step area acc) (processStep firstStep area (0, Map.empty))




