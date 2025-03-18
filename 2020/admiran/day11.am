|| day11.m


%export day11

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <avl>
%import <lens>
%import <map>
%import <maybe>
%import <mirandaExtensions>

m_mapAccumWithKey f s AVLLeaf = (s, AVLLeaf)
m_mapAccumWithKey f s (AVLNode (k, v) l r h)
    = (s3, AVLNode (k, v') l' r' h)
      where
        (s1, l') = m_mapAccumWithKey f s l
        (s2, v') = f s1 k v
        (s3, r') = m_mapAccumWithKey f s2 r

point == (int, int)

seatState ::= Floor | Empty | Occupied

isOccupied :: seatState -> bool
isOccupied Occupied = True
isOccupied _        = False

seatType :: char -> seatState
seatType '.' = Floor
seatType 'L' = Empty
seatType '#' = Occupied
seatType _   = error "seatType: bad input"

grid ::= Grid (m_map point seatState) (m_map point [point]) int int

g_seats  = Lens getf overf where getf (Grid a b c d) = a; overf fn (Grid a b c d) = Grid (fn a) b c d
g_seen   = Lens getf overf where getf (Grid a b c d) = b; overf fn (Grid a b c d) = Grid a (fn b) c d
g_maxRow = Lens getf overf where getf (Grid a b c d) = c; overf fn (Grid a b c d) = Grid a b (fn c) d
g_maxCol = Lens getf overf where getf (Grid a b c d) = d; overf fn (Grid a b c d) = Grid a b c (fn d)

showGrid :: grid -> string
showGrid grid
    = concatMap showRow [0 .. view g_maxRow grid]
      where
        showRow r   =  map (showCol r) [0 .. view g_maxCol grid] ++ "\n"
        showCol r c
            = case m_findWithDefault cmppoint Floor (r, c) (view g_seats grid) of
                Floor    -> '.'
                Empty    -> 'L'
                Occupied -> '#'

addSeen :: grid -> grid
addSeen grid
    = set g_seen seen grid
      where
        seen             = foldl addSeen' m_empty . m_keys . view g_seats $ grid
        addSeen' m p     = m_insert cmppoint p (seenSeats p) m
        seenSeats (r, c) = catMaybes . map (findSeat (r, c)) $ [(dr, dc) | dr <- [- 1 .. 1]; dc <- [-1 .. 1]; dr ~=0 \/ dc ~= 0]

        findSeat (r, c) (dr, dc)
            = Nothing,                    if r' < 0 \/ r' > view g_maxRow grid
            = Nothing,                    if c' < 0 \/ c' > view g_maxCol grid
            = findSeat (r', c') (dr, dc), if _eq cmpseatState Floor st
            = Just (r', c'),              otherwise
              where
                r' = r + dr
                c' = c + dc
                st = m_findWithDefault cmppoint Floor (r', c') (view g_seats grid)
        
occupiedAdjacent :: grid -> point -> int
occupiedAdjacent grid
    = length . filter isOccupied . adjacent
      where
        seats           = view g_seats grid
        adjacent (r, c) = map getSeat [(r', c') | r' <- [r - 1 .. r + 1]; c' <- [c - 1 .. c + 1]; r' ~= r \/ c' ~= c]
        getSeat p       = m_findWithDefault cmppoint Floor p seats

occupiedSeen :: grid -> point -> int
occupiedSeen grid
    = length . filter isOccupied . seen'
      where
        seats     = view g_seats grid
        seen      = view g_seen  grid
        seen' p   = map getSeat $ m_findWithDefault cmppoint [] p seen
        getSeat p = m_findWithDefault cmppoint Floor p seats

totalOccupied :: grid -> int
totalOccupied = length . filter (_eq cmpseatState Occupied) . m_elems . view g_seats

doRound1 :: (grid, bool) -> (grid, bool)
doRound1 (grid, _)
    = (set g_seats seats' grid, changed)
      where
        (changed, seats') = m_mapAccumWithKey checkSeat False (view g_seats grid)

        checkSeat changed p Empty    = (True, Occupied), if occupiedAdjacent grid p == 0
        checkSeat changed p Occupied = (True, Empty),    if occupiedAdjacent grid p >= 4
        checkSeat changed _ status   = (changed, status)

doRound2 :: (grid, bool) -> (grid, bool)
doRound2 (grid, _)
    = (set g_seats seats' grid, changed)
      where
        (changed, seats') = m_mapAccumWithKey checkSeat False (view g_seats grid)
        checkSeat changed p Empty    = (True, Occupied), if occupiedSeen grid p == 0
        checkSeat changed p Occupied = (True, Empty),    if occupiedSeen grid p >= 5
        checkSeat changed _ status   = (changed, status)

readGrid :: string -> io grid
readGrid fn
    = go <$>. lines <$>. readFile fn
      where
        go rows
            = foldl addSeat emptygrid $ gridList
              where
                emptygrid = Grid m_empty m_empty 0 0
                gridList  = [(seatType ch, r, c) | (r, row) <- enumerate rows; (c, ch) <- enumerate row]

       addSeat grid (Floor, r, c) = grid
       addSeat grid (st,    r, c)
           = over g_seats (m_insert cmppoint (r, c) st) . over g_maxRow (max2 cmpint r) . over g_maxCol (max2 cmpint c) $ grid

day11 :: io ()
day11
    = readGrid "../inputs/day11.txt" >>=. (go . addSeen)
      where
        go grid
            = io_mapM_ putStrLn [part1, part2]
              where
                (grid1, _) = hd . dropWhile snd . iterate doRound1 $ (grid, True)
                (grid2, _) = hd . dropWhile snd . iterate doRound2 $ (grid, True)
                part1      = (++) "part 1: " . showint . totalOccupied $ grid1
                part2      = (++) "part 2: " . showint . totalOccupied $ grid2
