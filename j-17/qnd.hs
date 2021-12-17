import qualified Data.Set as Set

type Velocity = (Int, Int)
type Point = (Int, Int)

type Area = (Point, Point)

main :: IO ()
main = do
  putStrLn "--- Example ---"
  resolve "example.txt" ((20, -5), (30, -10))
  putStrLn "--- DataSet 1 ---"
  resolve "input1.txt" ((96, -98), (125, -144))
  putStrLn "--- DataSet 2 ---"
  resolve "input2.txt" ((240, -57), (292, -90))


resolve :: FilePath -> Area -> IO ()
resolve nameFile area@((_, minY), (maxX, maxY)) = do
  putStr "  -- Part 1 > "
  print $ part1 area
  putStr "  -- Part 2 > "
  let l = createManyVelocityDumby maxX (maxY, abs maxY)
  print $ length . filter id $ map (canBeGood area) l

readInput :: FilePath -> IO  String
readInput = readFile

isInSpace :: Point -> Area -> Bool
isInSpace (x,y) ((a,b),(c,d)) = ((a <= x) && (x <= c)) && ((d <= y) && (y <= b))

nextPoint :: Point -> Velocity -> (Point, Velocity)
nextPoint (x, y) (vx, vy) = ((x + vx, y + vy),(if vx == 0 then 0 else vx-(vx `div` abs vx),  vy - 1))

canBeGood :: Area -> Velocity -> Bool
canBeGood a@((ax, ay), (bx, by)) = inner (0,0) where
  inner p@(x, y) v'@(vx, vy)
    | isInSpace p a = True
    | x > bx = False
    | y < by && vy <= 0 = False
    | otherwise = uncurry inner (nextPoint p v')

createManyVelocityDumby :: Int -> (Int, Int) -> [Velocity]
createManyVelocityDumby maxX (miny, maxy) = [(x, y) | x <- [0..maxX], y <- [miny..maxy]]

part1 :: Area -> Int 
part1 (_, (_, y)) = (abs y * (abs y - 1)) `div` 2 



