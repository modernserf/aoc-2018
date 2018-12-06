import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit, string)
import Text.Parsec.Combinator (many1)
import Control.Monad (void)
import Data.Either (rights)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)

main = do
    ls <- lines $ readFile "input-5.txt"
    let coords = rights $ fmap parseLine ls
    let finite = filter (isFinite coords) coords
    let filled = foldr (floodFill coords) [] finite
    let part1 = largestArea $ areas filled
    return part1

type ID = Int
type Area = Int
type Coord = (Int, Int)
type Bounds = (Coord, Coord)
type Territory = [(ID, Coord)]

-- parsing
parseLine = parse coordP ""

coordP :: Parser Coord
coordP = do
    x <- num
    void $ string ", "
    y <- num
    return (x, y)

num :: Parser Int
num = do
    n <- many1 digit
    return (read n)
    

boundsOf :: [Coord] -> Bounds
boundsOf coords = 
    let (xs, ys) = unzip coords in
        ((minimum xs, minimum ys), (maximum xs, maximum ys))

indexed1 = zip [1..]

floodFill :: [Coord] -> (ID, Coord) -> Territory -> Territory
-- in concentric squares around this point,
-- add a point to the territory with the current ID.
-- stop when a whole ring fails

closestCoord :: [Coord] -> Coord -> Coord
closestCoord coords x =
    snd $ maximumBy (compareF fst) zip (fmap (manhattanDistance x) coords) coords

compareF f l r = compare (f l) (f r)

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))

isFinite :: [Coord] -> Coord -> Boolean
-- if there exists a point to the NE, NW, SE, SW, then point is finite

largestArea :: IntMap Area -> Area
largestArea aMap = maximum $ fmap snd $ Map.toList aMap

areas :: Territory -> IntMap Area
areas = foldr addToMap Map.empty

addToMap :: Territory -> IntMap Area -> IntMap Area
addToMap (id, _) = Map.insertWith (+) id 1
    