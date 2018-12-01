import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.IntSet  as Set
import Data.Foldable (foldrM)

main = do
    ls <- fmap Text.lines $ Text.readFile "input-1.txt"
    let ints = fmap (toInt . Text.unpack) ls
    let part1 = sum ints
    let loop = ints ++ loop
    let (part2, _) = foldUntil findDup initState loop
    return (part1, part2)

toInt :: String -> Int
toInt ('+' : num) = read num
toInt ('-' : num) = - (read num)
toInt _ = 0

initState = (0, Set.empty)

foldUntil :: (a -> b -> Maybe b) -> b -> [a] -> b
foldUntil _ state [] = state
foldUntil f state (x : xs) =
    case f x state of
        Just next -> foldUntil f next xs
        Nothing -> state

findDup x (sum, visited) =
    if Set.member sum visited
    then Nothing
    else Just (sum + x, Set.insert sum visited)
