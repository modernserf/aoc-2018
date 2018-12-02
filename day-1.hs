import qualified Data.IntSet  as Set
import Data.Foldable (foldlM)
import Data.Either (fromLeft)

main = do
    ls <- lines <$> readFile "input-1.txt"
    let ints = fmap toInt ls
    let part1 = sum ints
    let loop = ints ++ loop
    let part2 = fromLeft 0 $ foldlM findDup initState loop
    return (part1, part2)

toInt :: String -> Int
toInt ('+' : num) = read num
toInt num = read num

initState = (0, Set.empty)

findDup (sum, visited) x =
    let next = sum + x in
    if Set.member next visited
    then Left next
    else Right (next, Set.insert next visited)
