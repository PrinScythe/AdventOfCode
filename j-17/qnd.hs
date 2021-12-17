import qualified Data.Set as Set

type Velocity = (Int, Int)
type Point = (Int, Int)

type Area = (Point, Point)

main :: IO ()
main = do
  let l = Set.fromList [(23,-10),(25,-9),(27,-5),(29,-6),(22,-6),(21,-7),(9,0),(27,-7),(24,-5),(25,-7),(26,-6),(25,-5),(6,8),(11,-2),(20,-5),(29,-10),(6,3),(28,-7),(8,0),(30,-6),(29,-8),(20,-10),(6,7),(6,4),(6,1),(14,-4),(21,-6),(26,-10),(7,-1),(7,7),(8,-1),(21,-9),(6,2),(20,-7),(30,-10),(14,-3),(20,-8),(13,-2),(7,3),(28,-8),(29,-9),(15,-3),(22,-5),(26,-8),(25,-8),(25,-6),(15,-4),(9,-2),(15,-2),(12,-2),(28,-9),(12,-3),(24,-6),(23,-7),(25,-10),(7,8),(11,-3),(26,-7),(7,1),(23,-9),(6,0),(22,-10),(27,-6),(8,1),(22,-8),(13,-4),(7,6),(28,-6),(11,-4),(12,-4),(26,-9),(7,4),(24,-10),(23,-8),(30,-8),(7,0),(9,-1),(10,-1),(26,-5),(22,-9),(6,5),(7,5),(23,-6),(28,-10),(10,-2),(11,-1),(20,-9),(14,-2),(29,-7),(13,-3),(23,-5),(24,-8),(27,-9),(30,-7),(28,-5),(21,-10),(7,9),(6,6),(21,-5),(27,-10),(7,2),(30,-9),(21,-8),(22,-7),(24,-9),(20,-6),(6,9),(29,-5),(8,-2),(27,-8),(30,-5),(24,-7)]
  putStrLn "--- Example ---"
  resolve "example.txt" l ((20, -5), (30, -10))
  putStrLn "--- DataSet 1 ---"
  resolve "input1.txt" l ((96, -98), (125, -144))
  putStrLn "--- DataSet 2 ---"
  resolve "input2.txt" l ((240, -57), (292, -90))


resolve :: FilePath -> Set.Set Velocity -> Area -> IO ()
resolve nameFile set area = do
  putStr "res : "
  print $ length . Set.filter id $ Set.map (canBeGood area) set

readInput :: FilePath -> IO  String
readInput = readFile

splitOn :: String -> String -> [String]
splitOn "" string = [string]
splitOn delimiters string = lines (map (\c -> if c `elem` delimiters then '\n' else c) string)


isInSpace (x,y) ((a,b),(c,d)) = (x >= a && x <= b) && (y>=b) && (y <= d)

nextPoint (x, y) (vx, vy) = ((x + vx, y + vy),(if vx == 0 then 0 else vx-vx `div` abs vx,  vy - 1))

canBeGood a v = inner a (0,0) v where
  inner area@(_, (bx, by)) p@(x, y) v@(vx, vy)
    | isInSpace p area = True
    | x > bx = False
    | y < by && vy <= 0 = False
    | otherwise = uncurry (inner a) (nextPoint p v)


