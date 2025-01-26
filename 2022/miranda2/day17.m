%export day17

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <lens>
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import <set>
%import <state> (>>=)/st_bind (>>)/st_right
%import <v2>


point == v2 int

|| shapes defined starting with 0,0 at bottom left, using V2 row col instead of V2 x y so that
|| the set ordering will order y (rows) first:
shapes :: [[point]]
shapes =
    [ [V2 0 0, V2 0 1, V2 0 2, V2 0 3]          || "horizontal bar"
    , [V2 0 1, V2 1 0, V2 1 1, V2 1 2, V2 2 1]  || "plus"
    , [V2 0 0, V2 0 1, V2 0 2, V2 1 2, V2 2 2]  || "j"
    , [V2 0 0, V2 1 0, V2 2 0, V2 3 0]          || "vertical bar"
    , [V2 0 0, V2 0 1, V2 1 0, V2 1 1]          || "square"
    ]

gameSt == (s_set point, [char], [point], [[point]])

|| lenses for gameSt
gs_rocks  = lensTup4_0  || set of points from dropped shapes (and initial floor)
gs_puffs  = lensTup4_1  || infinite stream of puffs
gs_shape  = lensTup4_2  || current shape
gs_shapes = lensTup4_3  || infinite stream of shapes

|| add current shape to the rocks and start a new shape
startNextShape :: gameSt -> gameSt
startNextShape st
    = (set gs_rocks rocks' . set gs_shape s . over gs_shapes tl) st
      where
        rocks'  = foldl (converse (s_insert cmppoint)) (view gs_rocks st) (view gs_shape st)
        h       = view lensV2_0 (s_last rocks')
        initLoc = V2 (h + 4) 2
        s       = (map (v2_add initLoc) . hd . view gs_shapes) st

puff :: gameSt -> gameSt
puff st
    = (set gs_shape s2 . over gs_puffs tl) st
      where
        p          = (hd . view gs_puffs) st
        s          = view gs_shape st
        hstep      = V2 0 1
        s1         = map ($v2_add hstep) s, if p ==. '>'
                   = map ($v2_sub hstep) s, otherwise
        cols       = map (view lensV2_1) s1
        rocks      = view gs_rocks st
        collision  = any (< 0) cols \/ any (> 6) cols \/ any (converse (s_member cmppoint) rocks) s1
        s2         = s,  if collision
                   = s1, otherwise

fall :: state gameSt bool
fall st
    = (True, st),                  if collision
    = (False, set gs_shape s' st), otherwise
      where
        vstep        = V2 1 0
        s'           = map ($v2_sub vstep) (view gs_shape st)
        rocks        = view gs_rocks st
        collision    = any (converse (s_member cmppoint) rocks) s'

getHeights :: int -> state gameSt [int]
getHeights n
    = st_liftA2 (:) getHeight (st_mapM (const go) [1 .. n])
      where
        go          = st_modify puff >> fall >>= check
        check False = go
        check True  = st_modify startNextShape >> getHeight
        getHeight   = st_get >>= (st_pure . view lensV2_0 . s_last . view gs_rocks)


|| find starting point and period length in height differences
findPeriod :: [int] -> (int, int)
findPeriod xs
    = go 0 m_empty (zipWith subtract xs (0 : xs))
      where
        go n seen xs
            = (n', n - n'),             if isJust mn'
            = go (n + 1) seen' (tl xs), otherwise
              where
                hseq  = take 25 xs
                mn'   = m_lookup (cmplist cmpint) hseq seen
                n'    = fromJust mn'
                seen' = m_insert (cmplist cmpint) hseq n seen

computeHeight :: int -> (int, int) -> [int] -> int
computeHeight n (s, p) hs
    = h1 + np * ph + pr
      where
        np = (n - s) $div p     || intber of full periods
        pp = (n - s) $mod p     || remainder of shapes to drop
        h1 = hs ! s             || height at beginning of period
        h2 = hs ! (s + p)       || height at end of period
        h3 = hs ! (s + pp)      || height of remainder
        ph = h2 - h1            || delta height of a full period
        pr = h3 - h1

day17 :: io ()
day17
    = readFile "../inputs/day17.txt" >>=. go
      where
        go jets
            = io_mapM_ putStrLn [part1, part2]
              where
                floor   = (s_fromList cmppoint . map (V2 0)) [0 .. 6]
                st      = startNextShape (floor, cycle jets, [], cycle shapes)
                heights = fst (getHeights 5000 st)
                part1   = (++) "part 1: " . showint $ heights ! 2022
                part2   = (++) "part 2: " . showint . computeHeight (10^12) (findPeriod heights) $ heights
