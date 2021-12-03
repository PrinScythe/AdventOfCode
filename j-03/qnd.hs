import Data.Char (digitToInt)
import Data.List (nubBy, partition)
import Data.Type.Equality (inner)

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
  print (part1 content)
  putStr "Part Two : "
  print (part2 content)

readInput :: FilePath -> IO [[Int]]
readInput nameFile = do
  content <- readFile nameFile
  let temp = lines content
  let res = map (map digitToInt) temp
  return res

part1 :: [[Int]] -> Int
part1 inputs = epsilon * gamma where
  epsilon = binaryToDecimal (inverse binaryGamma)
  gamma =  binaryToDecimal binaryGamma
  binaryGamma = exctractGamma inputs

exctractGamma :: [[Int]] -> [Int]
exctractGamma l =   map (foo (<) (length l)) (foldl1 (zipWith (+)) l)

exctractEpsilon :: [[Int]] -> [Int]
exctractEpsilon l =   map (foo (>) (length l)) (foldl1 (zipWith (+)) l)

inverse :: [Int] -> [Int]
inverse = map (\b -> if b == 0 then 1 else 0)

binaryToDecimal :: Num t => [t] -> t
binaryToDecimal l = tailrec 0 0 l' where
  l' = reverse l
  tailrec pos acc l'' = case l'' of
    [] -> acc
    (x:xs) -> tailrec (pos + 1) (acc + x * 2 ^ pos) xs

part2 :: [[Int]] -> Int
part2 inputs = oxygen * co2 where 
  oxygen = binaryToDecimal (findValue fst (>) inputs)
  co2 = binaryToDecimal (findValue snd (<) inputs)

findValue :: (([[Int]], [[Int]]) -> [[Int]]) -> (Int -> Int -> Bool) -> [[Int]] -> [Int]
findValue ifEqualThen comparator l = innerFunc ifEqualThen comparator 0 (nubBy compareBinary l) where
  innerFunc ifEqualThen comparator pos [] = []
  innerFunc ifEqualThen comparator pos [x] = x
  innerFunc ifEqualThen comparator pos l' = innerFunc ifEqualThen comparator (pos + 1) l'' where
    l''
      | length (fst s) == length (snd s) = ifEqualThen s
      | length (fst s) `comparator` length (snd s) = fst s
      | otherwise = snd s
    s = partition (\b -> (b !! pos) == 1) l'

foo :: (Num t1, Num p) => (t1 -> t2 -> Bool) -> t2 -> t1 -> p
foo comparator len x = if (x * 2) `comparator` len then 0 else 1

compareBinary :: Eq a => [a] -> [a] -> Bool
compareBinary a b = all (==True) (zipWith (==) a b )




