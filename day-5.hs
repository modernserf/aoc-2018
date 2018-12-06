import Data.Char (isAsciiUpper, toUpper)
import Data.Foldable (minimumBy)
import Data.Ord (compare)

main = do
  str <- readFile "input-5.txt"
  let part1 = length $ collapsePolymer str
  let withRemoved = testPolymersWithLettersRemoved str
  let part2 = entryForMinValue withRemoved
  return (part1, part2)

letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

testPolymersWithLettersRemoved :: String -> [(Char, Int)]
testPolymersWithLettersRemoved polymer = 
  zip
    letters
    (length <$> collapsePolymer <$> (filterLetters polymer) <$> letters)

filterLetters polymer letter = filter (((/=) letter) . toUpper) polymer

collapsePolymer :: String -> String
collapsePolymer = collapse [] 

collapse :: [Char] -> String -> String
collapse out [] = out
collapse []  (i : ins) = collapse [i] ins
collapse (o : outs) (i : ins) = 
  if opposite o i then collapse outs ins else collapse (i : o : outs) ins


opposite :: Char -> Char -> Bool
opposite a b = 
  case (isAsciiUpper a, isAsciiUpper b, toUpper a == toUpper b) of
    (True, False, True) -> True
    (False, True, True) -> True
    _ -> False

entryForMinValue :: [(t, Int)] -> (t, Int)
entryForMinValue lst = minimumBy sortPairByValue lst 
sortPairByValue (_, l) (_, r) = compare l r