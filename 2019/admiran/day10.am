%export day10

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <mirandaExtensions>


point == (int, int)

scanObj ::=
    ScanObj
    int     || so_quadrant
    int     || so_x
    int     || so_y

toScanObj :: point -> point -> scanObj
toScanObj (x1, y1) (x2, y2)
    = ScanObj quad dx dy
      where
        dx = x2 - x1
        dy = y2 - y1
        quad = 0, if dx >= 0 & dy < 0
             = 1, if dx > 0 & dy >= 0
             = 2, if dx <= 0 & dy > 0
             = 3, otherwise

fromScanObj :: point -> scanObj -> point
fromScanObj (x1, y1) (ScanObj q2 x2 y2)
    = (x1 + x2, y1 + y2)

scanList :: [scanObj] -> ([scanObj], [scanObj])
scanList = go
           where
             go [] = ([], [])
             go (seen : sos) = ((seen : vis), (blk ++ blkd))
                               where
                                 (blk, sos') = span (isEQ . angleCompare seen) sos
                                 (vis, blkd) = go sos'

isEQ, notEQ :: ordering -> bool
isEQ EQ  = True
isEQ _   = False

notEQ EQ = False
notEQ _  = True


soCompare :: scanObj -> scanObj -> ordering
soCompare so1 so2
    = angleCmp, if notEQ angleCmp
    = distCmp,  otherwise
      where
        (ScanObj _ x1 y1) = so1
        (ScanObj _ x2 y2) = so2
      angleCmp            = angleCompare so1 so2
      distCmp             = compare cmpint (x1 ^ 2 + y1 ^ 2) (x2 ^ 2 + y2 ^ 2)

angleCompare :: scanObj -> scanObj -> ordering
angleCompare (ScanObj q1 x1 y1) (ScanObj q2 x2 y2)
    = quadCmp,  if notEQ quadCmp
    = EQ,       if x1 == 0 & x2 == 0
    = EQ,       if y1 == 0 & y2 == 0
    = LT,       if y1 == 0
    = GT,       if y2 == 0
    = LT,       if x1 == 0
    = GT,       if x2 == 0
    = compare cmpint (y1 * x2) (y2 * x1), otherwise
      where
        quadCmp = compare cmpint q1 q2

asteroidMap == [point]

scanFrom :: asteroidMap -> point -> ([scanObj], [scanObj])
scanFrom asteroids station = (scanList . sortBy soCompare . map (toScanObj station) . filter (_ne cmppoint station)) asteroids

countVisible :: asteroidMap -> point -> int
countVisible asteroids station = (length . fst) (scanFrom asteroids station)

vaporize :: asteroidMap -> point -> [scanObj]
vaporize asteroids station
    = go (scanFrom asteroids station)
      where
        go (vaporized, [])      = vaporized
        go (vaporized, blocked) = vaporized ++ go (scanList blocked)

readAsteroidMap :: string -> io asteroidMap
readAsteroidMap fn
    = go <$>. lines <$>. readFile fn
      where
        go rows
            = [(x, y) | (y, row) <- enumerate rows; (x, ch) <- enumerate row; ch ==. '#']

                
day10 :: io ()
day10
    = readAsteroidMap "../inputs/day10.input" >>=. go
      where
        go asteroids
            = io_mapM_ putStrLn [part1, part2]
              where
                station = maxBy cmpint (countVisible asteroids) asteroids
                (x, y)  = fromScanObj station . hd . drop 199 . vaporize asteroids $ station
                part1   = (++) "part 1: " . showint . countVisible asteroids $ station
                part2   = (++) "part 2: " . showint $ 100 * x + y
