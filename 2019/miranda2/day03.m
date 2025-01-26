%export day03

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <maybe>
%import <mirandaExtensions>


point == (int, int)

distanceTo :: point -> point -> int
distanceTo (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)


dir ::= U | D | L | R

segment ::=
    Segment
        dir
        int || pos
        int || min
        int || max
        int || steps

path  == [(dir, int)]
wire  == [segment]

intersection :: segment -> segment -> maybe point
intersection (Segment dh y minx maxx sh) (Segment dv x miny maxy sv)
    = Nothing,     if y < miny
    = Nothing,     if y > maxy
    = Nothing,     if x < minx
    = Nothing,     if x > maxx
    = Just (x, y), otherwise

intersectTime :: segment -> segment -> maybe int
intersectTime hs vs
    = Nothing, if isNothing ix
    = Just t,  otherwise
      where
        Segment dh _ minh maxh th  = hs
        Segment dv _ minv maxv tv  = vs
        ix                         = intersection hs vs
        Just (x, y)                = ix
        deltah                     = case dh of R -> x - minh; L -> maxh - x; _ -> error "deltah is not R or L"
        deltav                     = case dv of U -> y - minv; D -> maxv - y; _ -> error "deltav is not U or D"
        t                          = th + tv + deltah + deltav

allPairs :: [*] -> [(*, *)]
allPairs []             = []
allPairs [x]            = []
allPairs (x1 : x2 : xs) = (x1, x2) : allPairs (x1 : xs) ++ allPairs (x2 : xs)

findAll :: (segment -> segment -> maybe *) -> [(wire, wire)] -> [*]
findAll fn
    = concat . map wi . allPairs
      where
        wi (w1, w2) = catMaybes [fn hs vs | hs <- fst w1; vs <- snd w2] ++
                      catMaybes [fn hs vs | hs <- fst w2; vs <- snd w1]

traceFrom :: int -> point -> path -> (wire, wire)
traceFrom time pos path
    = ws
      where
        (ws, _, _)                         = foldl trace (([], []), time, pos) path
        trace ((hs, vs), t, (x, y)) (U, n) = ((hs, Segment U x y (y + n) t : vs), t + n, (x, y + n))
        trace ((hs, vs), t, (x, y)) (D, n) = ((hs, Segment D x (y - n) y t : vs), t + n, (x, y - n))
        trace ((hs, vs), t, (x, y)) (L, n) = ((Segment L y (x - n) x t : hs, vs), t + n, (x - n, y))
        trace ((hs, vs), t, (x, y)) (R, n) = ((Segment R y x (x + n) t : hs, vs), t + n, (x + n, y))

readPaths :: string -> io [path]
readPaths fn
    = map readPath <$>. lines <$>. readFile fn
      where
        readPath          = map readMove . split ','
        readMove []       = error "readMove []"
        readMove (c : cs) = (mkDir c, intval cs)
        mkDir 'U' = U
        mkDir 'D' = D
        mkDir 'L' = L
        mkDir 'R' = R
        mkDir _   = error "readPaths: bad direction"

day03 :: io ()
day03
    = readPaths "../inputs/day03.input" >>=. go
      where
        go paths
            = io_mapM_ putStrLn [part1, part2]
              where
                port  = (0, 0)
                wires = map (traceFrom 0 port) paths
                inter = findAll intersection wires
                times = findAll intersectTime wires
                part1 = (++) "part 1: " . showint . min cmpint . map (distanceTo port) $ inter
                part2 = (++) "part 2: " . showint . min cmpint $ times
