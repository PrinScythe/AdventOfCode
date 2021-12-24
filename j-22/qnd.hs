
import qualified Data.Map.Lazy as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

type Area = (Range, Range, Range)
type Range = (Int, Int)
type Step = (Action, Area)
type Action = Int
type Point = (Int, Int, Int)
type Cuboid = Map.Map Point Action
type Parallelepipede = (Point, Point, Action)

getSommets :: Parallelepipede -> [Point]
getSommets ((x1, y1, z1), (x2, y2, z2), _) = [(x, y, z)| x <- [x1,x2], y <- [y1, y2], z <- [z1, z2]]

estCeDansLeParallelepipede :: Parallelepipede -> Point -> Bool
estCeDansLeParallelepipede ((x1, y1, z1), (x2, y2, z2), _) (x, y, z) = x1 <= x && x <= x2 && y1 <= y && y <= y2 && z1 <= z && z <= z2

getSommetsInclusDans :: Parallelepipede -> Parallelepipede -> [Point]
getSommetsInclusDans p1 = filter (estCeDansLeParallelepipede p1) . getSommets 

getVolume :: Point -> Point -> Int
getVolume (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) * abs (y1 - y2) * abs (z1 - z2)

getVolumeParallelepipede :: Parallelepipede -> Int
getVolumeParallelepipede (p1, p2, _) = getVolume p1 p2

volumeIntersect p1 p2 = res (getSommetsInclusDans p1 p2) (getSommetsInclusDans p2 p1) where 
  res s1 s2= case (length s1, length s2) of
    (0, 0) -> 0
    (1, _) -> getVolume (head s1) (head s2)
    (2, 2) -> getVolume (head s1) (last s1)
    (_, 8) -> getVolumeParallelepipede p2
    (8, _) -> getVolumeParallelepipede p1
    (4, 4) -> getVolume (head s1) ()

pointOppose2D :: Point -> [Point] -> Point
pointOppose2D pointReference (p : l) = if length (difference pointReference p) == 2 then p else pointOppose2D pointReference l
pointOppose2D pointReference [] = pointReference

pointOppose3D :: Point -> [Point] -> Point
pointOppose3D pointReference (p : l) = if length (difference pointReference p) == 3 then p else pointOppose3D pointReference l
pointOppose3D pointReference [] = pointReference

difference :: Point -> Point -> [Int]
difference (x1, y1, z1) (x2, y2, z2) = filter (/=0) [x1 - x2, y1 - y2, z1 - z2]




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
  print $ fst $ processSteps (extractStep "off x=-50..50,y=-50..50,z=-50..50") steps

readInput :: FilePath -> IO  [Step]
readInput nfile = do
  content <- readFile nfile
  return (map extractStep . lines $ content)

splitOn :: String -> String -> [String]
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

processStep :: Step -> (Int, Cuboid) -> (Int, Cuboid)
processStep (action, ((x1, x2), (y1, y2), (z1, z2))) (numberOn, m)
 | null m = ((x2 - x1 + 1) * (y2 - y1 + 1) * (z2 - z1 + 1) * action, Map.fromList [((x, y, z), action) | x <- [x1..x2], y<-[y1..y2], z<-[z1..z2]])
 | otherwise = Map.foldlWithKey foldOp (numberOn, m) (Map.fromList [((x, y, z), action) | x <- [x1..x2], y<-[y1..y2], z<-[z1..z2]]) where
    foldOp acc@(n, space) p action = case space Map.!? p of
      Nothing -> acc
      Just a
        | a == action -> acc
        | action == 0 -> (n - 1, Map.insert p action space)
        | otherwise -> (n + 1, Map.insert p action space)

processSteps :: Step  -> [Step] -> (Int, Cuboid)
processSteps firstStep = foldl (flip processStep) (processStep firstStep (0, Map.empty))







