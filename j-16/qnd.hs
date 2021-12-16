
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text.Internal.Builder as Set
import Text.XHtml.Frameset (width)
import Text.XHtml.Strict (height)

type Bit = Char
type Bits = String
type Version = Integer
type TypeID = Integer 
type LenghtTypeID = Integer
type Packet = Bits

data Madness = Literal Version TypeID [Packet] | Operator Version TypeID Bool LenghtTypeID [Madness] deriving Show 


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
  let translateTable = Map.fromList [('0',"0000"), ('1',"0001"), ('2',"0010"), ('3',"0011"), ('4',"0100"), ('5',"0101"), ('6',"0110"), ('7',"0111"), ('8',"1000"), ('9',"1001"), ('A',"1010"), ('B',"1011"), ('C',"1100"), ('D',"1101"), ('E',"1110"), ('F',"1111")]
  original <- readInput nameFile
  let translation = translate original translateTable
  putStr "Part One : "
  print translation

readInput :: FilePath -> IO  String
readInput = readFile

splitOn :: String -> [Char] -> [String]
splitOn "" string = [string]
splitOn delimiters string = lines (map (\c -> if c `elem` delimiters then '\n' else c) string)

translate :: String -> Map.Map Char String  -> String
translate str translateTable = concatMap (translateTable Map.!) str

bitsToInt :: Bits -> Integer 
bitsToInt b = snd $ foldr (\x (pow, acc) -> (pow * 2, read [x] * pow + acc) ) (1, 0) b

constructMadness :: Bits -> Madness
constructMadness bits = res where
  res = Literal 2 3 []
  version = bitsToInt $ take 3 bits 
  typeCode = bitsToInt . take 3 . drop 3 $ bits
  bool = if typeCode == 4 then Nothing else Just ((bits !! 6) == "0")
  maybeLenghtTypeId = if typeCode == 4 then 





