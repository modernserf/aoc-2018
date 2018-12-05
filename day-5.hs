import Data.Char (isAsciiUpper, toUpper)
import Data.Foldable (minimumBy)
import Data.Ord (compare)

main = do
  str <- readFile "input-5.txt"
  let part1 = length $ collapsePolymer str
  let withRemoved = testPolymersWithLettersRemoved str
  let part2 = entryForMinValue withRemoved
  return (part1, withRemoved, part2)

letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

testPolymersWithLettersRemoved :: String -> [(Char, Int)]
testPolymersWithLettersRemoved polymer = 
  zip
    letters
    (length <$> collapsePolymer <$> (filterLetters polymer) <$> letters)

filterLetters polymer letter = filter (\ch -> (toUpper ch) /= letter) polymer

collapsePolymer :: String -> String
collapsePolymer x =
  let collapsed = collapsePolymer1 x in
    if collapsed == x 
      then collapsed 
      else collapsePolymer collapsed

collapsePolymer1 :: String -> String
collapsePolymer1 [] = []
collapsePolymer1 [a] = [a]
collapsePolymer1 (a : b : cs) =
  if opposite a b then collapsePolymer1 cs else a : collapsePolymer1 (b : cs)

opposite :: Char -> Char -> Bool
opposite a b = 
  case (isAsciiUpper a, isAsciiUpper b, toUpper a == toUpper b) of
    (True, False, True) -> True
    (False, True, True) -> True
    _ -> False

entryForMinValue :: [(t, Int)] -> (t, Int)
entryForMinValue lst = minimumBy sortPairByValue lst 
sortPairByValue (_, l) (_, r) = compare l r