import qualified Data.Map.Strict as Map
import qualified Data.IntSet as Set
import Data.IntSet ((\\))
import Data.Either (rights)
import Text.Parsec (ParseError, parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, digit)
import Text.Parsec.Combinator (many1)
import Control.Monad (void)

main = do
    ls <- lines <$> readFile "input-3.txt"
    let claims = rights (fmap parseLine ls)
    let allIDs = Set.fromList $ fmap (\(Claim id _ _) -> id ) claims 
    let accumulatedClaims = accumulated claims
    let overlapping = filter hasOverlap $ Map.elems accumulatedClaims
    let part1 = length overlapping

    let overlappingIDs = foldr Set.union Set.empty overlapping
    let part2 = allIDs \\ overlappingIDs
    return (part1, part2)

data Claim = Claim { claimID :: Int
                   , pos :: (Int, Int)
                   , size :: (Int, Int)
                   } deriving (Show)
                   

type ClaimCounter = Map.Map (Int, Int) Set.IntSet

examples = ["#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2"]

-- counting
populatedSquares :: Claim -> [(Int, Int, Int)]
populatedSquares (Claim { claimID = id, pos = (x, y), size = (w, h) }) =
  [(id, dx, dy) | dx <- [x..(x + w - 1)], dy <- [y..(y + h - 1)]]

accumulateClaims :: (Int, Int, Int) -> ClaimCounter -> ClaimCounter
accumulateClaims (claimID, x, y) = 
  Map.insertWith Set.union (x, y) (Set.singleton claimID)

allSquares :: [Claim] -> [(Int, Int, Int)]
allSquares = concat . (fmap populatedSquares)
accumulated = (foldr accumulateClaims Map.empty) . allSquares

hasOverlap set = Set.size set > 1

-- parsing
parseLine = parse claim "" 

claim :: Parser Claim
claim = do
  void $ char '#'
  claimID <- num
  void $ char ' '
  void $ char '@'
  void $ char ' '
  claimX <- num
  void $ char ','
  claimY <- num
  void $ char ':'
  void $ char ' '
  claimW <- num
  void $ char 'x'
  claimH <- num
  return $ Claim claimID (claimX, claimY) (claimW, claimH)

num :: Parser Int
num = do
    n <- many1 digit
    return (read n)
  