|| day17.m -- Clumsy Crucible


%export day17

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <heap>
%import <lens>
%import <maybe>
%import <mirandaExtensions>
%import <set>
%import <vector>


loc  == (int, int)

dist :: loc -> loc -> int
dist (r1, c1) (r2, c2) = abs (r2 - r1) + abs (c2 - c1)


dir ::= N | S | E | W | X       || X represents no current dir (at start)

|| list of next directions we can move, given the current direction
|| the first direction is equal to the original direction, which will be removed
|| if the step count is 0
nextDirs :: dir -> [dir]
nextDirs N = [N, E, W]
nextDirs S = [S, E, W]
nextDirs E = [E, N, S]
nextDirs W = [W, N, S]
nextDirs X = [N, S, E, W]

step :: dir -> loc -> loc
step N (r, c) = (r - 1, c)
step S (r, c) = (r + 1, c)
step E (r, c) = (r, c + 1)
step W (r, c) = (r, c - 1)
step X loc    = loc


grid * ::= Grid (vector *) int int      || linear vector of the grid points, width, height

g_at :: grid * -> loc -> *
g_at (Grid v w _) (r, c) = v_index v $ r * w + c

crucible     == (loc, dir, int)
crucibleType == (int, int)    || max consecutive in one dir, min consecutive in one dir
move         == (int, crucible)

|| lenses for move
m_heatLoss = lensFst
m_crucible = lensSnd
m_loc      = composeLens m_crucible lensTup3_0
m_dir      = composeLens m_crucible lensTup3_1
m_steps    = composeLens m_crucible lensTup3_2


expand :: grid int -> crucibleType -> move -> [move]
expand g (dirMax, dirMin) (hl, (loc, dir, steps))
    = catMaybes . map move $ vndirs
      where        
        Grid _ w h = g

        || valid next directions, checking the steps count against the crucibleType to limit or force turns
        vndirs
            = tl ndirs,   if steps == 0                 || must turn
            = [hd ndirs], if steps > dirMax - dirMin    || cannot turn
            = ndirs,      otherwise                     || can turn or not
              where ndirs = nextDirs dir

        onGrid (r, c)
            = 0 <= r < h & 0 <= c < w

        move dir'
            = Just m', if onGrid loc' & steps' >= 0
            = Nothing, otherwise
              where
                loc'   = step dir' loc
                hl'    = g_at g loc'
                steps' = steps - 1,  if _eq cmpdir dir dir'     || moving in the same direction
                       = dirMax - 1, otherwise                  || changed direction
                m'     = (hl + hl', (loc', dir', steps'))

|| solve using Dijkstra's algorithm with a priority heap
solve :: grid int -> crucibleType -> loc -> loc -> int
solve g (dirMax, dirMin) start finish
    = go (h_singleton startm) s_empty
      where
        startm     = (0, (start, X, 0))
        atFinish m = _eq cmploc finish (view m_loc m) & view m_steps m < dirMax - dirMin
        cmp m1 m2  = cmpint (view m_heatLoss m1) (view m_heatLoss m2)

        go h seen
            = error "no solution found", if h_null h
            = view m_heatLoss curr,      if atFinish curr
            = go h1 seen,                if s_member cmpcrucible currCru seen
            = go h2 seen',               otherwise
              where
                (curr, h1) = fromJust $ h_viewMin cmp h
                currCru    = view m_crucible curr
                seen'      = s_insert cmpcrucible currCru seen
                h2         = foldr (h_insert cmp) h1 . expand g (dirMax, dirMin) $ curr

readMap :: string -> io (grid int)
readMap fn
    = (go . lines) <$>. readFile fn
      where
        go rows
            = Grid v w h
              where
                v = v_fromList . map digitVal . concat $ rows
                h = #rows
                w = #(hd rows)

day17 :: io ()
day17
    = readMap "../inputs/day17.txt" >>=. go
      where
        go g
            = io_mapM_ putStrLn [part1, part2]
              where
                Grid _ w h = g
                part1      = ("part 1: " ++) . showint $ solve g (3, 0)  (0, 0) (h - 1, w - 1)
                part2      = ("part 2: " ++) . showint $ solve g (10, 4) (0, 0) (h - 1, w - 1)
