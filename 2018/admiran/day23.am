|| day23.m


%export day23

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <astar>
%import <lens>
%import <maybe>
%import <mirandaExtensions>
%import <parser> (>>=)/p_bind (<$>)/p_fmap (<*)/p_left (*>)/p_right
%import <v3>


|| integer power of 2 >= N, integer log2 n
pow2, log2 :: int -> int
pow2 n
    = 1,                   if n <= 0
    = 2 * pow2 (n $div 2), otherwise

log2 n
    = 0,                   if n <= 0
    = 1 + log2 (n $div 2), otherwise


coordinate == v3 int

origin :: coordinate
origin = v3_pure 0

nanoBot ::= NanoBot coordinate int      || n_coord, n_radius

n_coord    (NanoBot c r)   = c
n_radius   (NanoBot c r)   = r
n_vertices (NanoBot c r)   = [v3_add c v | n <- [-r, r]; v <- [V3 n 0 0, V3 0 n 0, V3 0 0 n]]
n_contains (NanoBot c r) p = v3_dist c p <= r


boundingBox ::= BoundingBox coordinate int [nanoBot]     || corner, edge, bots

|| custom ordI for boundingBox that ignores the nanobot list
cmpboundingBox (BoundingBox c1 e1 bs1) (BoundingBox c2 e2 bs2) = cmpcoordinate c1 c2 $thenCmp cmpint e1 e2

b_corner   (BoundingBox c e bs) = c;
b_edge     (BoundingBox c e bs) = e;
b_bots     (BoundingBox c e bs) = bs;
b_center   (BoundingBox c e bs) = v3_add c . v3_pure $ e $div 2
b_radius   (BoundingBox c e bs) = e $div 2
b_vertices (BoundingBox c e bs) = [v3_add c (V3 x y z) | x <- [0, e]; y <- [0, e]; z <- [0, e]]
b_contains (BoundingBox c e bs) p
    = v3_cmp (<=) c p & v3_cmp (<) p (v3_add c (v3_pure e))
      where
        v3_cmp op a b = v3_foldr (&) True $ v3_liftA2 op a b

boundingBoxEnclosing :: [nanoBot] -> boundingBox
boundingBoxEnclosing nBots
    = BoundingBox minCoord (pow2 maxEdge) nBots
      where
        verts    = concatMap n_vertices nBots
        minCoord = foldr1 (v3_min cmpint) verts
        maxCoord = foldr1 (v3_max cmpint) verts
        maxEdge  = v3_foldl (max2 cmpint) (viewV3_0 delta) delta where delta = v3_sub maxCoord minCoord

partitionBox :: boundingBox -> [boundingBox]
partitionBox (BoundingBox corner edge bots)
    = map cullBots parts
      where
        (V3 cx cy cz) = corner
        r             = edge $div 2
        parts         = [BoundingBox (V3 x y z) r bots | x <- [cx, cx + r]; y <- [cy, cy + r]; z <- [cz, cz + r]]    
        cullBots bb   = BoundingBox (b_corner bb) (b_edge bb) $ filter (intersects bb) (b_bots bb)

intersects :: boundingBox -> nanoBot -> bool
intersects bb nb
    = bb $b_contains n_coord nb            \/
      nb $n_contains b_center bb           \/
      any (bb $b_contains) (n_vertices nb) \/
      any (nb $n_contains) (b_vertices bb)

coordinateWithMostIntersections :: boundingBox -> coordinate
coordinateWithMostIntersections bb
    = b_corner . last $ path
      where
        (path, _)        = aStarSolve cmpboundingBox bb goal expandFn () moveCost (const 0)
        goal bb          = b_edge bb == 0
        expandFn (bb, _) = (partitionBox bb, ())

        moveCost bb1 bb2
            = (numBots bb1 - numBots bb2) * 100 + distance bb2 - distance bb1
              where
                numBots     = length . b_bots
                distance bb = log2 . v3_dist origin . b_center $ bb


p_bot :: parser nanoBot
p_bot
    = (p_string "pos=<" *> p_intlist) >>= checkList
      where
        checkList [x, y, z] = NanoBot (V3 x y z) <$> (p_string ">, r=" *> p_int <* p_spaces)
        checkList _         = p_fail

readBots :: string -> io [nanoBot]
readBots fn
    = go <$>. parse (p_some p_bot) <$>. readFile fn
      where
        go (mbots, ps) = fromMaybe (error (p_error ps)) mbots

day23 :: io ()
day23
    = readBots "../inputs/day23.input" >>=. go
      where
        go nBots
            = io_mapM_ putStrLn [part1, part2]
              where 
                strongest = maxBy cmpint n_radius nBots
                inRange   = filter (n_contains strongest . n_coord) nBots
                result    = coordinateWithMostIntersections . boundingBoxEnclosing $ nBots
                part1     = (++) "part 1: " . showint . length $ inRange
                part2     = (++) "part 2: " . showint . v3_dist origin $ result
