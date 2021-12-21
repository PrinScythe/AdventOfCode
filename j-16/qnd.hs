
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text.Internal.Builder as Set
import Text.XHtml.Frameset (width)
import Text.XHtml.Strict (height)

type Bit = Char
type Bits = String
type Version = Int
type TypeID = Int
type Lenght = Int
type Packet = Bits
type Operation = [Madness] -> Int
type Value = Int

data Madness = Literal Version Value | Operator Operation Version TypeID Bool Lenght [Madness] 

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
  let translateTable = Map.fromList [('0',"0000"), ('1',"0001"), ('2',"0010"), ('3',"0011"), ('4',"0100"), ('5',"0101"), ('6',"0110"), ('7',"0111"), ('8',"1000"), ('9',"1001"), ('A',"1010"), ('B',"1011"), ('C',"1100"), ('D',"1101"), ('E',"1110"), ('F',"1111")]
  original <- readInput nameFile
  let translation = translate original translateTable
  putStr "Part One : "
  let (rest, madness) = constructMadness translation
  print $ sumVersions madness
  putStr "Part Two : "
  print $ compute madness

readInput :: FilePath -> IO  String
readInput = readFile

splitOn :: String -> [Char] -> [String]
splitOn "" string = [string]
splitOn delimiters string = lines (map (\c -> if c `elem` delimiters then '\n' else c) string)

translate :: String -> Map.Map Char String  -> String
translate str translateTable = concatMap (translateTable Map.!) str

bitsToInt :: Bits -> Int
bitsToInt b = snd $ foldr (\x (pow, acc) -> (pow * 2, read [x] * pow + acc) ) (1, 0) b

constructMadness :: Bits -> (Bits, Madness)
constructMadness bits = case bitsToInt . take 3 . drop 3 $ bits of
  4 -> constructLiteral bits
  0 ->  constructOperator bits 0 (sum . map compute)
  1 ->  constructOperator bits 1 (product . map compute)
  2 ->  constructOperator bits 2 (minimum . map compute)
  3 ->  constructOperator bits 3 (maximum . map compute)
  5 ->  constructOperator bits 5 (comparatorOperation (>) . map compute)
  6 ->  constructOperator bits 6 (comparatorOperation (<)  . map compute)
  7 ->  constructOperator bits 7 (comparatorOperation (==)  . map compute)
  _ -> error "I believed in you ... Y-Y"

constructLiteral :: Bits -> (Bits, Madness)
constructLiteral bits = (rest, Literal version (bitsToInt $ concat subpacket)) where
  version = bitsToInt $ take 3 bits
  (rest, subpacket) = getLiteralSubPacket (drop 6 bits)

getLiteralSubPacket :: Bits -> (Bits, [Packet])
getLiteralSubPacket bits = tailrec bits [] where
   tailrec b subpacket = if head b == '0'
     then (drop 5 b, subpacket ++ [take 4 (drop 1 b)])
     else tailrec (drop 5 b) (subpacket ++ [take 4 (drop 1 b)])

constructOperator :: Bits -> TypeID -> Operation -> (Bits, Madness)
constructOperator bits typeId operation = res where
  version = bitsToInt $ take 3 bits
  bool = (bits !! 6) == '0'
  len = if bool then bitsToInt . take 15 . drop 7 $ bits else bitsToInt . take 11 . drop 7 $ bits
  res = if bool then (drop (22 + len) bits, Operator operation version typeId bool len (getOperatorSubPacket0 (take len . drop 22 $ bits)))
                else (rest, Operator operation version typeId bool len subpacket)
  (rest, subpacket) = getOperatorSubPacket1 (drop 18 bits) len

getOperatorSubPacket1 :: Bits -> Int -> (Bits, [Madness])
getOperatorSubPacket1 bits len = tailrec len bits [] where
  tailrec 0 bits madnesses = (bits, madnesses)
  tailrec len bits madnesses = uncurry (tailrec (len - 1)) $ (\ (x, y) -> (x, madnesses ++ [y])) (constructMadness bits) 


getOperatorSubPacket0 :: Bits -> [Madness]
getOperatorSubPacket0 bits = tailrec bits [] where
  tailrec [] madnesses = madnesses
  tailrec bits madnesses = uncurry tailrec $ (\ (x, y) -> (x,  madnesses ++ [y])) (constructMadness bits) 


sumVersions :: Madness -> Int
sumVersions madness = case madness of 
  Literal version _ -> version
  Operator _ version _ _ _ madnesses -> version + (sum . map sumVersions $ madnesses)

compute :: Madness -> Int
compute m = case m of
  Literal _ value-> value
  Operator op _ _ _ _ childs -> op childs


comparatorOperation :: (Int -> Int -> Bool) -> [Int] -> Int
comparatorOperation o l = if o (head l) (last l) then 1 else 0