|| day12.m -- Garden Groups


%export day12

%import "adventLib"
%import <lens>
%import <maybe>
%import <set>
%import <state>
%import <v2>            (|+|)/v2_add (|*|)/v2_mul
%import <vector>


pos  == v2 int
dir  == v2 int
plot == (pos, char)

|| check if positions are orthogonal (same row or column)
orthoTo :: pos -> pos -> bool
orthoTo (V2 a b) (V2 c d) = a == c \/ b == d

grid     == vector char
gridInfo == (grid, int, int)

makeGridInfo :: string -> gridInfo
makeGridInfo s
    = (v, nrows, ncols)
      where
        rows  = lines s
        v     = concat rows |> v_fromList
        nrows = #rows
        ncols = #(hd rows)

|| return char at pos on grid, or '?' if off-grid
(!?) :: gridInfo -> pos -> char
(g, nr, nc) !? V2 r c
    = g !! (r * nc + c), if 0 <= r < nr & 0 <= c < nc
    = '?',               otherwise

|| since the seen "set" is implemented as an mvector, we need to thread
|| reads and writes of it through a state monad, so we implement a scanSt
|| to handle all the reads and update of the visit and seen info
scanSt == (s_set pos, mvector bool)

|| lenses for scanSt
visit   = lensFst
seen    = lensSnd

|| lift lens operations into state
st_view lns   st = (view lns st, st)
st_over lns f st = ((), over lns f st)
st_set  lns x st = ((), set  lns x st)

|| operations on visit list in scanSt
nextVisit :: state scanSt (maybe pos)
nextVisit
    = st_view visit >>= s_viewMin .> check
      where
        check Nothing        = st_pure Nothing
        check (Just (p, ps)) = st_pure (Just p) << st_set visit ps

|| operations on seen vector in scanSt
isSeen, notSeen :: gridInfo -> pos -> state scanSt bool
isSeen (_, _, nc) (V2 r c) = st_view seen >>= ($v_read (r * nc + c))

notSeen gi p = not <$> isSeen gi p

setSeen :: gridInfo -> pos -> state scanSt ()
setSeen (_, _, nc) (V2 r c) = st_view seen >>= set where set v = v_write v (r * nc + c) True


|| find all on-grid adjacent plots
|| note: expand order is (-1, -1), (-1, 0), (-1, 1), ... so that the expand index can be used to select a particular known pos
expand :: gridInfo -> pos -> [plot]
expand gi p = [(p', gi !? p') | p' <- [p |+| V2 dr dc | dr, dc <- [-1, 0, 1]; dr ~= 0 \/ dc ~= 0]]


|| counting all possible corners in a 3x3 view
|| a pattern is an 8-element list of compare operations
pattern == [(char -> char -> bool)]

cornerCases, peninsulaCases :: [pattern]
(singletonCase, cornerCases, peninsulaCases)
    = (spat, rot4 ipat ++ rot4 opat, rot4 ppat)
      where
        ne a b = a ~=. b
        eq a b = a ==. b
        dc a b = True

        rot [a, b, c, d, e, f, g, h] = [f, d, a, g, b, h, e, c]
        rot _ = undef

        rot4 p = iterate rot p |> take 4

        spat = [ dc, ne, dc
               , ne,     ne
               , dc, ne, dc]

        ipat = [ ne, eq, dc
               , eq,     dc
               , dc, dc, dc]

        opat = [ dc, eq, dc
               , eq,     ne
               , dc, ne, dc]

        ppat = [ dc, ne, dc
               , ne,     ne
               , dc, eq, dc]

countCorners :: plot -> [plot] -> int
countCorners (p, c) plts
    = count checkCase cornerCases + count checkCase peninsulaCases * 2 + if' (checkCase singletonCase) 4 0
      where
        cs          = map snd plts              || extract all the adjacent plot plants
        checkCase k = and $ zipWith (c |>) k cs || apply the pattern k checking the center plant against the adjacent ones

regionInfo == (int, int, int)   || area, perimeter, #sides

|| scan a new region starting at plot p, returning the regionInfo
|| with updated visit and seen info
scanRegion :: gridInfo -> pos -> state scanSt regionInfo
scanRegion gi p
    = go 0 0 0 [p]
      where
        c = gi !? p

        go a r s [] = st_pure (a, r, s)
        go a r s (p : ps)
            = notSeen gi p >>= checkVisit
              where
                checkVisit False = go a r s ps
                checkVisit True  = setSeen gi p >> (expand gi p |> update)

                update adj
                    = st_filterM (notSeen gi) toVisit >>= (st_over visit . addAll) >>   || add plots not in this group to the visit set
                      st_filterM (notSeen gi) toScan  >>= calc                          || update the calculation for area, perim, etc.
                      where                                                             || and continue with unseen plots in our group
                        inReg   = [p' | (p', c') <- adj; c' ==. c]                      || all adjacent plots that have the same plant as this one
                        toVisit = [p' | (p', c') <- adj; c' ~=. c & c' ~=. '?']         || all adjacent plots with a different plant (but on grid)
                        toScan  = filter (orthoTo p) inReg                              || inReg plots we can step to next (orthogonal direction)

                        addAll xs s = foldr (s_insert cmppos) s xs
                        calc   xs   = go (a + 1) (r + perim toScan) (s + countCorners (p, c) adj ) (ps ++ xs)
                        perim  xs   = 4 - #xs

|| scan the grid, reporting part1 and part2 costs
scanGrid :: gridInfo -> v2 int
scanGrid gi
    = st_evalState (go v0) initSt
      where
        v0     = v2_pure 0
        initSt = (s_singleton v0, initSeen gi)

        initSeen (_, nr, nc) = v_rep (nr * nc) False |> v_unsafeThaw

        go tally
            = nextVisit >>= checkDone
              where
                checkDone Nothing  = st_pure tally
                checkDone (Just p) = scanRegion gi p >>= computeInfo

                computeInfo (a, r, s) = go (tally |+| (V2 a a |*| V2 r s))

|| 809352 is too low for part 2
day12 :: io ()
day12
    = readFile "../inputs/day12.txt" >>= makeGridInfo .> go
      where
        go gi
            = output [part1, part2]
              where
                V2 c1 c2 = scanGrid gi
                part1    = showint  c1
                part2    = showint  c2
