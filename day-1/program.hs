import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.Set     as Set

main = do
    ls <- fmap Text.lines $ Text.readFile "input-1.txt"
    let ints = fmap (toInt . Text.unpack) ls
    let loop = ints ++ loop
    let value = firstDuplicate loop 0 Set.empty
    return value

toInt :: String -> Int
toInt ('+' : num) = read num
toInt ('-' : num) = - (read num)
toInt _ = 0

firstDuplicate (x : xs) value visited = 
    let nextValue = x + value
    in
    if Set.member nextValue visited
    then nextValue
    else firstDuplicate xs nextValue $ Set.insert nextValue visited