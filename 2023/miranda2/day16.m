|| day16.m -- The Floor Will Be Lava
||
|| part 2 needs to be solved with memoization; need to figure out how to split up paths through the grid
|| problem is that following a beam will eventually encounter a visited beam state, but we might not want
|| to stop there if we are trying to tally the total energized set from a particular beam loc (splitter / reflector) and dir

%export day16

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <lens>
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import <set>
%import <state> (>>=)/st_bind
%import <v2> (<+>)/v2_add


loc  == v2 int
dir  == v2 int
beam == (loc, dir)

|| lenses for beam
b_loc = lensFst
b_dir = lensSnd
b_x   = composeLens b_loc lensV2_0
b_y   = composeLens b_loc lensV2_1
b_h   = composeLens b_dir lensV2_0
b_v   = composeLens b_dir lensV2_1

grid      == m_map loc char

reflector :: char -> bool
reflector '/'  = True
reflector '\\' = True
reflector c    = False

v2_swap :: v2 * -> v2 *
v2_swap (V2 a b) = V2 b a

refl1, refl2 :: beam -> beam
refl1 = over b_dir v2_swap
refl2 = over b_dir (v2_neg . v2_swap)

reflc :: char -> beam -> beam
reflc '\\' = refl1
reflc '/'  = refl2
reflc _    = error "bad char for reflc"

splitc :: char -> beam -> [beam]
splitc '-' b = [b],                if view b_v b == 0
             = [refl1 b, refl2 b], otherwise
splitc '|' b = [b],                if view b_h b == 0
             = [refl1 b, refl2 b], otherwise
splitc _ _   = error "bad char for splitc"

step :: beam -> beam
step (loc, dir) = (loc <+> dir, dir)

onGrid :: int -> int -> beam -> bool
onGrid w h b
    = 0 <= view b_x b < w &
      0 <= view b_y b < h


gridSt == (grid, int, int, s_set beam, s_set loc)      || grid, width, height, visitdir, visited

|| lenses for gridSt
gs_grid     = lensTup5_0
gs_width    = lensTup5_1
gs_height   = lensTup5_2
gs_visitdir = lensTup5_3
gs_visited  = lensTup5_4

interact :: beam -> state gridSt [beam]
interact b gs
    = ([], gs),   if out \/ seen
    = (bs', gs'), otherwise
      where
        loc  = view b_loc b
        c    = fromMaybe '.' $ m_lookup cmploc loc (view gs_grid gs)
        out  = ~onGrid (view gs_width gs) (view gs_height gs) b
        seen = s_member cmpbeam b (view gs_visitdir gs)
        gs'  = over gs_visited (s_insert cmploc loc) . over gs_visitdir (s_insert cmpbeam b) $ gs
        bs'  = map step bs

        bs   = [b],          if c ==. '.'
             = [reflc  c b], if reflector c
             = splitc c b,   otherwise

|| run a beam through the grid and tally the total energized 
energizedCount :: gridSt -> beam -> int
energizedCount gs b
    = s_size . view gs_visited . st_execState (go [b]) $ gs
      where
        go []       = st_pure ()
        go (b : bs) = interact b >>= go . (++ bs)

|| find the maximum energized, starting the beam at any location on the perimeter
maxEnergized :: gridSt -> int
maxEnergized gs
    = max cmpint . map (energizedCount gs) $ (top ++ bottom ++ left ++ right)
      where
        wlim   = view gs_width  gs - 1
        hlim   = view gs_height gs - 1
        top    = [(V2 x    0,    V2 0    1)    | x <- [0 .. wlim]]
        bottom = [(V2 x    hlim, V2 0    (-1)) | x <- [0 .. wlim]]
        left   = [(V2 0    y,    V2 1    0)    | y <- [0 .. hlim]]
        right  = [(V2 hlim y,    V2 (-1) 0)    | y <- [0 .. wlim]]

readGrid :: string -> io (grid, int, int)
readGrid fn
    = (go . lines) <$>. readFile fn
      where
        go rows
            = (g, width, height)
              where
                height = #rows
                width  = #(hd rows)
                g      = m_fromList cmploc [(V2 x y, c) | (y, row) <- enumerate rows; (x, c) <- enumerate row]

day16 :: io ()
day16
    = readGrid "../inputs/day16.txt" >>=. go
      where
        go (g, w, h)
            = io_mapM_ putStrLn [part1, part2]
              where
                initSt    = (g, w, h, s_empty, s_empty)
                initBeam  = (V2 0 0, V2 1 0)
                part1 = ("part 1: " ++) . showint $ energizedCount initSt initBeam
                part2 = ("part 2: " ++) . showint $ maxEnergized initSt
