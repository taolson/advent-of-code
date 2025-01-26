%export day24

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <mirandaExtensions>
%import <set>


gridType ::= Flat | Recursive

point == (int, int)
grid  == (s_set point)

gridLocations, gridDirections :: [point]

gridLocations = [(x, y) | y,x <- [0 .. 4]]
gridDirections = [(-1, 0), (1, 0), (0, -1), (0, 1)]

gridVal :: grid -> point -> num
gridVal grid loc = 1, if s_member cmppoint loc grid
                 = 0, otherwise

adjacent :: gridType -> grid -> grid -> grid -> point -> [(grid, point)]
adjacent gt grid gridUp gridDn loc
    = concatMap (locate loc) gridDirections
      where
        locate (x, y) (dx, dy)
            = [(grid,   (x', y'))], if _eq cmpgridType gt Flat
            = [],                   if center
            = [(gridUp, (1,  2 ))], if x' < 0
            = [(gridUp, (3,  2 ))], if x' > 4
            = [(gridUp, (2,  1 ))], if y' < 0
            = [(gridUp, (2,  3 ))], if y' > 4
            = downX 0,              if center' & dx == 1
            = downX 4,              if center' & dx == -1
            = downY 0,              if center' & dy == 1
            = downY 4,              if center' & dy == -1
            = [(grid,   (x', y'))], otherwise
              where
                x'         = x + dx
                y'         = y + dy
                center     = x  == 2 & y  == 2
                center'    = x' == 2 & y' == 2
                downX x    = map (setY gridDn x) [0 .. 4]
                downY y    = map (setX gridDn y) [0 .. 4]
                setY g x y = (g, (x, y))
                setX g y x = (g, (x, y))

adjacentCount :: gridType -> grid -> grid -> grid -> point -> num
adjacentCount gt grid gridUp gridDn loc
    = (sum . map (uncurry gridVal)) (adjacent gt grid gridUp gridDn loc)

evolveGrid :: gridType -> grid -> grid -> grid -> grid
evolveGrid gt grid gridUp gridDn
    = foldl addBug s_empty gridLocations
      where
        addBug grid' loc = s_insert cmppoint loc grid',      if evolveBug loc
                         = grid',                            otherwise
        evolveBug loc    = count loc == 1,                   if s_member cmppoint loc grid
                         = count loc == 1 \/ count loc == 2, otherwise
        count            = adjacentCount gt grid gridUp gridDn

evolve :: gridType -> [grid] -> [grid]
evolve gt grids
    = (strip . go) padded
      where
        go (u : c : d : r) = evolveGrid gt c u d : go (c : d : r)
        go gs              = []
        padded             = (pad . pad) grids, if _eq cmpgridType gt Recursive
                           = pad grids,         otherwise
        pad gs             = [s_empty] ++ gs ++ [s_empty]
        strip [g]          = [g]
        strip gs           = strip' (tl gs),    if s_null (hd gs)
                           = strip' gs,         otherwise
        strip' [g]         = [],                if s_null g
                           = [g],               otherwise
        strip' (g : gs)    = g : strip' gs
        strip' _           = error "evolve: strip []"


firstDuplicate :: [[grid]] -> grid
firstDuplicate
    = go s_empty
      where
        go seen ([g] : gs) = g,                       if s_member cmpgrid g seen
                           = go (s_insert cmpgrid  g seen) gs, otherwise
        go _ _             = error "firstDuplicate: bad gridlist"

bioRating :: grid -> num
bioRating grid
    = (fst . foldl addPow2 (0, 1) . map (gridVal grid)) gridLocations
      where
        addPow2 (rating, pow2) bug = (rating + bug * pow2, pow2 * 2)

readGrid :: string -> io grid
readGrid fn
    = go <$>. lines <$>. readFile fn
      where
        go rows = foldl (converse (s_insert cmppoint)) s_empty [(x, y) | (y, row) <- enumerate rows; (x, c) <- enumerate row; c ==. '#']

day24 :: io ()
day24
    = readGrid "../inputs/day24.input" >>=. go
      where
        go grid
            = io_mapM_ putStrLn [part1, part2]
              where
                dup   = (firstDuplicate . iterate (evolve Flat)) [grid]
                rgrid = (hd . drop 200 . iterate (evolve Recursive)) [grid]
                part1 = (++) "part 1: " . showint . bioRating $ dup
                part2 = (++) "part 2: " . showint . sum . map s_size $ rgrid
