import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Foldable (foldlM, foldrM)
import Data.Either (fromLeft)
import Data.List (find)

main = do
    ls <- lines <$> readFile "input-2.txt"
    let part1 = checksum ls
    let part2 = findMatch ls
    return (part1, part2)

example1 = ["abcdef","bababc","abbcde","abcccd","aabcdd","abcdee","ababab"]
example2 = ["abcde","fghij","klmno","pqrst","fguij","axcye","wvxyz"]

letterCount str = foldr incLetter Map.empty str
incLetter ch map = Map.insertWith (+) ch 1 map
elemsSet m = Set.fromList $ Map.elems m
has2or3 elems = (bit $ Set.member 2 elems, bit $ Set.member 3 elems)
bit x = if x then 1 else 0

checksum xs =
    let (twos, threes) = unzip $ fmap (has2or3 . elemsSet . letterCount) xs in
    (sum twos) * (sum threes)

findMatch [] = ""
findMatch (source : toCompare) =
    case foldlM strsAreClose source toCompare of
        Left match -> match
        Right _ -> findMatch toCompare

strsAreClose source compare =
    maybe (Right source) (Left . fst) $ foldrM compareChars ("", 0) (zip source compare)

-- note: this folds from the right, and builds the match string backwards
compareChars (l, r) (match, errors) =
    case (l == r, errors) of
        (True, _) -> Just (l : match, errors)
        (False, 0) -> Just (match, 1)
        _ -> Nothing
