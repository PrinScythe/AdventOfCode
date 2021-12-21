import qualified Data.Set as Set
import qualified Data.IntMap.Strict as IMap
import qualified Data.Map.Strict as Map
import qualified Data.Maybe
import Data.List (foldl')
import Data.Tuple (swap)


main :: IO ()
main = do
  putStrLn "--- Example ---"
  resolve 4 8
  putStrLn "--- DataSet 1 ---"
  resolve 7 1
  putStrLn "--- DataSet 2 ---"
  resolve 10 9


resolve start1 start2 = do
  let resPlayer1 = map (\x -> (x + start1 - 1) `mod` 10 + 1) [6,10,2,2,10]
  let resPlayer2 = map (\x -> (x + start2 - 1) `mod` 10 + 1) [5,8,9,8,5,10,3,4,3,10]
  let diceRolls = IMap.fromListWith (+) [(x+y+z, 1)| x<-[1..3], y<-[1..3], z<-[1..3]]
  putStr "  -- Part 1 > "
  print $ computeLooser (resPlayer1, resPlayer2)
  putStr "  -- Part 2 > "
  print $ quantumRec start1 start2 0 0 diceRolls
  print $ snd $ quantumRec' start1 start2 0 0 Map.empty diceRolls

computeScoreTurn :: Int -> [Int] -> Int -> Int -> Int
computeScoreTurn turn res score playerTurn = score + c where
  t = (turn - 1) `mod` (5 * playerTurn)
  c = res !! t

computeLooser :: ([Int], [Int]) -> Int
computeLooser (r1, r2) = tailrec (0,0) (r1, r2) 1 1 where
  tailrec (score1, score2) (res1, res2) turn curPlayer
    | score2 >= 1000 = score1 * (turn * 6 - (if curPlayer == 2 then 3 else 0))
    | otherwise = tailrec (score2, computeScoreTurn turn res1 score1 curPlayer) (res2, res1) (if curPlayer == 2 then turn + 1 else turn) (curPlayer `mod` 2 + 1)

-- Partie Deux sans mémoisation
quantumRec :: IMap.Key -> IMap.Key -> IMap.Key -> IMap.Key -> IMap.IntMap Int -> (Int, Int)
quantumRec p1 p2 s1 s2  diceRolls =
  if s2 >= 21 then (0,1)
  else  swap $ IMap.foldlWithKey'
       (\(t1, t2) k v -> (\(u1, u2) -> (t1 + u1 * v, t2 + u2 * v)) (quantumRec p2 (np p1 k) s2 (s1 + np p1 k) diceRolls))
         (0,0) diceRolls
     where
      np p k = (p + k - 1) `mod` 10 + 1


-- Partie Deux avec mémoisation
quantumRec' :: IMap.Key -> IMap.Key -> IMap.Key -> IMap.Key -> Map.Map (IMap.Key, IMap.Key, IMap.Key, IMap.Key) (Int, Int) -> IMap.IntMap Int -> (Map.Map (IMap.Key, IMap.Key, IMap.Key, IMap.Key) (Int, Int), (Int, Int))
quantumRec' p1 p2 s1 s2 memo diceRolls =
  if s2 >= 21 then (memo, (0,1))
  else case memo Map.!? (p1, p2, s1, s2) of
    Just x -> (memo, x)
    Nothing -> (\(m, (x, y)) -> (Map.insert (p1, p2, s1, s2) (y, x) m, (y, x)))
      $ IMap.foldlWithKey'
       (\(m, (t1, t2)) k v -> (\(m, (u1, u2)) -> (m, (t1 + u1 * v, t2 + u2 * v))) (quantumRec' p2 (np p1 k) s2 (s1 + np p1 k) m diceRolls))
         (memo, (0, 0)) diceRolls
     where
      np p k = ((p + k - 1) `mod` 10) + 1
      