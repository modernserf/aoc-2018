import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Foldable (foldlM)

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
    let matchResults = foldlM strsAreClose source toCompare in
    case matchResults of
        Left match -> match
        Right _ -> findMatch toCompare

strsAreClose :: String -> String -> Either String String
strsAreClose source compare =
    let (match, errors) = foldl compareChars ("", 0) (zip source compare) in
    if errors == 1 then Left match else Right source

-- perf TODO: bail at 2 errors, build string in reverse then flip
compareChars :: (String, Int) -> (Char, Char) -> (String, Int)
compareChars (match, errors) (l, r) =
    if l == r then (match ++ [l], errors) else (match, errors + 1)