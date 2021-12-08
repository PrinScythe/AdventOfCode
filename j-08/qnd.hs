import Data.Map.Lazy(Map, empty, insertWith, (!))
import Data.List (intersect)
import Text.ParserCombinators.Parsec (digit)

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
  content <- readInput nameFile
  putStr "Part One : "
  print (part1 content)
  putStr "Part Two : "
  print (part2 content)

readInput :: FilePath -> IO [String]
readInput nameFile = do
  content <- readFile nameFile
  return (lines content)

splitOn :: String -> String -> [String]
splitOn "" string = [string]
splitOn delimiters string = lines (map (\c -> if c `elem` delimiters then '\n' else c) string)

part1 :: [String] -> Int
part1 = length 
  . filter (\cur -> length cur == 2 || length cur == 3 || length cur == 4 || length cur == 7)
  . concatMap (words . (last . splitOn "|"))

part2 :: [String] -> Int
part2 = sum . map computeNumberDisplayed

computeNumberDisplayed :: String -> Int
computeNumberDisplayed line = deductNumberFrom decoder digits where
  decoder = makeDecoder encodedSignals
  digits = last splitRes
  encodedSignals = head splitRes
  splitRes = map words $ splitOn "|" line


deductNumberFrom :: Map Int [Char] -> [String] -> Int
deductNumberFrom decoder = foldl (\acc cur -> acc * 10 + decode decoder cur) 0

decode :: Map Int [Char] -> String -> Int
decode decoder word
  | length word == 2 = 1
  | length word == 3 = 7
  | length word == 4 = 4
  | length word == 7 = 8
  | length word == 5 && (length ((decoder ! 1) `intersect` word) == 2) = 3
  | length word == 5 && (length ((decoder ! 4) `intersect` word) == 2) = 2
  | length word == 5 = 5
  | length word == 6 && (length ((decoder ! 1) `intersect` word) == 1) = 6
  | length word == 6 && (length ((decoder ! 4) `intersect` word) == 4) = 9
  | length word == 6 = 0
  | otherwise = error "Oops : Wrong Input"

makeDecoder :: [[Char]] -> Map Int [Char]
makeDecoder = foldl completeDecoder empty

completeDecoder :: Map Int [Char] -> [Char] -> Map Int [Char]
completeDecoder decoder word
  | length word == 2 = foldl (associateLetterWithNumber 1) decoder word
  | length word == 4 = foldl (associateLetterWithNumber 4) decoder word
  | otherwise = decoder

associateLetterWithNumber :: Int -> Map Int [Char] -> Char -> Map Int [Char]
associateLetterWithNumber number decoder letter = insertWith (++) number [letter] decoder