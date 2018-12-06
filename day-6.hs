main = do
    ls <- lines $ readFile "input-5.txt"
    let coords = fmap parse ls
    let bounds = boundsOf coords
    let filled = floodFill coords bounds
    let part1 = largestFiniteArea (areas filled) (finiteIDs coords bounds)
    return part1

type Coord = (Int, Int)
type Bounds = (Coord, Coord)

parse :: String -> Coord

boundsOf :: [Coord] -> Bounds
boundsOf coords = 
    let (xs, ys) = unzip coords in
        ((minimum xs, minimum ys), (maximum xs, maximum ys))

indexed1 = zip [1..]

floodFill :: [Coord] -> Bounds -> [(Int, Coord)]
floodFill coords ((minX, minY), (maxX, maxY)) =
    [(i, (x, y)) 
        | x <- [minX .. maxX]
        , y <- [minY .. maxY]
        , i <- closest (indexed1 coords) (x, y)]


closest :: [(Int, Coord)] -> Coord -> Int 

largestFiniteArea :: Map Int Int -> Set Int -> Int
largestFiniteArea aMap finiteSet =
    maximum $ fmap snd $ filter (\(id, _) -> Set.member id finiteSet) $ Map.toList aMap

areas :: [(Int, Coord)] -> Map Int Int
areas = foldr addToMap Map.empty

finiteIDs :: [Coord] -> Bounds -> Set Int

-- key = ID, value = count
addToMap :: [(Int, Coord)] -> Map Int Int -> Map Int Int
addToMap (id, _) = Map.insertWith (+) id 1
    