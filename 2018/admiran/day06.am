|| day06.m


%export day06

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import <parser> (<$>)/p_fmap (<*)/p_left (*>)/p_right


point == (int, int)

distanceTo :: point -> point -> int
distanceTo (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

pointType ::= Shared | Internal point | External point

grid ::= Grid [point] int int int int

coord :: grid -> [point]
coord (Grid ps _ _ _ _) = ps

minX, maxX, minY, maxY :: grid -> int
minX (Grid ps nx mx ny my) = nx
maxX (Grid ps nx mx ny my) = mx
minY (Grid ps nx mx ny my) = ny
maxY (Grid ps nx mx ny my) = my

makeGrid :: [point] -> grid
makeGrid points
    = Grid points minx maxx miny maxy
      where
        p : ps = points
        (x, y) = p

        (minx, maxx, miny, maxy) = foldl go (x, x, y, y) ps

        go (nx, mx, ny, my) (x, y)
            = (min2 cmpint nx x, max2 cmpint mx x, min2 cmpint ny y, max2 cmpint my y)

closestCoordinates :: grid -> point -> (point, [point])
closestCoordinates grid point
    = (point, collect . group . distances $ grid)
      where
        collect   = map snd
        group     = hd . groupBy cmpfst . sortOn cmpint fst where cmpfst a b = fst a == fst b
        distances = map addDist . coord where addDist c = (distanceTo point c, c)

doPart1 :: grid -> int
doPart1 grid
    = m_foldr (max2 cmpint) 0 . m_fmap (fromMaybe 0) $ tallyInternal
      where
        gridPoints                = [(x, y) | x <- [minX grid .. maxX grid]; y <- [minY grid .. maxY grid]]
        tallyInternal             = foldl tallyCoord m_empty classifiedPoints
        tallyCoord m Shared       = m
        tallyCoord m (Internal c) = m_insertWith cmppoint (mb_liftA2 (+)) c (Just 1) m
        tallyCoord m (External c) = m_insert cmppoint c Nothing m

        classifiedPoints = map (classify . closestCoordinates grid) $ gridPoints

        classify ((x, y), (c : cs))
            = Shared,     if #cs > 0
            = External c, if x == minX grid \/ x == maxX grid \/ y == minY grid \/ y == maxY grid
            = Internal c, otherwise

        classify p = error "classify: empty coordinates"

doPart2 :: grid -> int -> int
doPart2 grid maxTotalDist
    = length . filter (< maxTotalDist) . map sumDistance $ gridPoints
      where
        sumDistance pt = sum . map (distanceTo pt) . coord $ grid
        gridPoints = [(x, y) | x <- [minX grid .. maxX grid]; y <- [minY grid .. maxY grid]]

p_point :: parser point
p_point = p_liftA2 pair p_int (p_char ',' *> p_spaces *> p_int) <* p_spaces

p_points :: parser [point]
p_points = p_some p_point

readPoints :: string -> io [point]
readPoints fn
    = go <$>. parse p_points <$>. readFile fn
      where
        go (mpoints, ps) = fromMaybe (error (p_error ps)) mpoints

day06 :: io ()
day06
    = readPoints "../inputs/day06.input" >>=. (go . makeGrid)
      where
        go grid
            = io_mapM_ putStrLn [part1, part2]
              where
                part1 = (++) "part 1: " . showint . doPart1 $ grid
                part2 = (++) "part 2: " . showint . doPart2 grid $ 10_000
