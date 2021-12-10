import Data.List(sort)

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
part1 = sum . foldl foo [] where
  foo acc string = case findIllegal string of
    Nothing -> acc
    Just (_, 'i') -> acc
    Just ('i', x) -> error "test"
    Just (_, x) -> illegalScoreForChar x :acc

part2 :: [String] -> Int
part2 s = allScores !! (length allScores `div` 2) where 
  allScores = sort  (foldl foo [] s )
  foo acc cur = case complete cur of
    Nothing -> acc
    Just completionPart -> computeCompletionScore completionPart : acc

findIllegal :: String -> Maybe (Char, Char)
findIllegal = tailrec [] where
  startChars = ['(', '[', '{', '<']
  tailrec [] [] = Nothing
  tailrec (x:_) [] = Just ('i', 'i')
  tailrec [] (x:t) = if x `elem` startChars then tailrec [x] t else Just ('i', x)
  tailrec pile@(y:t2) (x:t1)
    | isCorrect y x = tailrec t2 t1
    | x `elem` startChars = tailrec (x:pile) t1
    | otherwise = Just (y, x)


complete :: String -> Maybe String
complete = tailrec [] where
  startChars = ['(', '[', '{', '<']
  tailrec [] [] = Nothing
  tailrec l [] = Just (completion l [])
  tailrec [] (x:t) = if x `elem` startChars then tailrec [x] t else Nothing
  tailrec pile@(y:t2) (x:t1)
    | isCorrect y x = tailrec t2 t1
    | x `elem` startChars = tailrec (x:pile) t1
    | otherwise = Nothing
  completion [] l = l
  completion (x:t) l | x == '(' = completion t (l ++ ")")
  completion (x:t) l | x == '{' = completion t (l ++ "}")
  completion (x:t) l | x == '[' = completion t (l ++ "]")
  completion (x:t) l | x == '<' = completion t (l ++ ">")

computeCompletionScore :: String -> Int
computeCompletionScore = foldl (\acc cur -> (acc * 5) + completeScoreTable cur) 0

completeScoreTable :: Char -> Int
completeScoreTable ')' = 1
completeScoreTable ']' = 2
completeScoreTable '}' = 3
completeScoreTable '>' = 4

isCorrect :: Char -> Char -> Bool
isCorrect a b
  | a == '(' && b == ')' = True
  | a == '{' && b == '}' = True
  | a == '[' && b == ']' = True
  | a == '<' && b == '>' = True
  | otherwise  = False


expectedChar :: Char -> Char
expectedChar '[' = ']'
expectedChar '(' = ')'
expectedChar '{' = '}'
expectedChar '<' = '>'
expectedChar _ = error "invalid"

illegalScoreForChar :: Char -> Int
illegalScoreForChar ')' = 3
illegalScoreForChar ']' = 57
illegalScoreForChar '}' = 1197
illegalScoreForChar '>' = 25137
