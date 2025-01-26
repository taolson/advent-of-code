%export day24

%import <astar>
%import <avl>
%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <lens>
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import <v2>


location  == v2 int
intLstMap == m_map int ([int], [int])           || map from a row/col index to a list of inital cols/rows
grid      == (intLstMap, intLstMap, int, int)   || vert and horiz blizzards, #rows, #cols in grid

|| lenses for grid
gd_vert  = lensTup4_0
gd_horiz = lensTup4_1
gd_rows  = lensTup4_2
gd_cols  = lensTup4_3

emptyGrid :: grid
emptyGrid = (m_empty, m_empty, 0, 0)

collision :: grid -> location -> int -> bool
collision (vert, horiz, rows, cols) (V2 r c) t
    = (any (== r) . map (($mod rows) . (subtract t))) up   \/
      (any (== r) . map (($mod rows) . (+ t)))        down \/
      (any (== c) . map (($mod cols) . (subtract t))) left \/
      (any (== c) . map (($mod cols) . (+ t)))        right
        where
          (up, down)    = fromMaybe ([], []) (m_lookup cmpint c vert)
          (left, right) = fromMaybe ([], []) (m_lookup cmpint r horiz)

move == (int, location)

solve :: grid -> int -> location -> location -> int
solve g t start goal
    = #path - 1
      where
        (path, ())      = aStarSolve cmpmove (t, start) isGoal exp () (const . const 1) est
        rows            = view gd_rows g
        cols            = view gd_cols g
        isGoal (t, loc) = _eq cmplocation loc goal
        est (t, loc)    = v2_dist goal loc

        exp ((t, loc), ())
            = (map (pair t') locs, ())
              where
                V2 r c        = loc
                t'            = t + 1
                locs          = filter valid [loc, V2 (r - 1) c, V2 (r + 1) c, V2 r (c - 1), V2 r (c + 1)]
                valid loc     = inBounds loc & ~collision g loc t'
                inBounds loc  = 0 <= r < rows & 0 <= c < cols \/ _eq cmplocation loc start \/ _eq cmplocation loc goal where V2 r c = loc

|| create a grid, which is a pair of mappings from a row/col to a corresponding list of cols/rows of
|| initial blizzard locations.  The two maps hold vertical moving (up, down) and horizontal moving (left, right)
|| initial locations. This allows a quick lookup of all blizzards that could interact at a particular row/col
|| (mapping a +t / -t mod size to the list of initial cols/rows to get the position at time t).
readGrid :: string -> io grid
readGrid fn
    = go <$>. lines <$>. readFile fn
      where
        go input
            = (over gd_rows (+ 1) . over gd_cols (+ 1)) g
              where
                g = foldl ins emptyGrid [(d, r, c) |
                        (r, row) <- zip2 [-1 ..] input;   || start at -1 to ignore the top edge chars
                        (c, d)   <- zip2 [-1 ..] row;     || start at -1 to ignore the left edge char
                        member cmpchar "<>^v" d]

        ins g (d, r, c)
            = (over gd_vert  (m_insertWith cmpint apPair c ([r], [])) . over gd_rows (max2 cmpint r) . over gd_cols (max2 cmpint c)) g, if d ==. '^'
            = (over gd_vert  (m_insertWith cmpint apPair c ([], [r])) . over gd_rows (max2 cmpint r) . over gd_cols (max2 cmpint c)) g, if d ==. 'v'
            = (over gd_horiz (m_insertWith cmpint apPair r ([c], [])) . over gd_rows (max2 cmpint r) . over gd_cols (max2 cmpint c)) g, if d ==. '<'
            = (over gd_horiz (m_insertWith cmpint apPair r ([], [c])) . over gd_rows (max2 cmpint r) . over gd_cols (max2 cmpint c)) g, if d ==. '>'
            = error "readGrid: bad direction character",                                                                                otherwise

        apPair (a, b) (c, d) = (a ++ c, b ++ d)

day24 :: io ()
day24
    = readGrid "../inputs/day24.txt" >>=. go
      where
        go g
            = io_mapM_ putStrLn [part1, part2]
              where
                start = V2 (-1) 0
                goal  = V2 (view gd_rows g) (view gd_cols g - 1)
                t1    = solve g 0  start goal
                t2    = t1 + solve g t1 goal start
                t3    = t2 + solve g t2 start goal
                part1 = "part 1: " ++ showint t1
                part2 = "part 2: " ++ showint t3
