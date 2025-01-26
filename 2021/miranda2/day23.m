%export day23

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <heap>
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import <set>


position == (int, int)

dist :: position -> position -> int
dist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

|| amphipods are Amber, Bronze, Copper, or Desert
amphiType ::= A | B | C | D

amphiTypes :: [amphiType]
amphiTypes = [A, B, C, D]

moveEnergy :: amphiType -> int
moveEnergy A = 1
moveEnergy B = 10
moveEnergy C = 100
moveEnergy D = 1000

|| amphipods are in one of 3 states, either their initial starting position,
|| have moved into the hallway, or have moved to their final position
amphiState ::= Start | Hall | Finish

amphipod == (amphiType, amphiState, position)

amphiPos :: amphipod -> position
amphiPos (t, s, p) = p

|| the burrow consists of the locations of the amphipods
burrow == s_set amphipod

|| the home room X locations for each amphiType
homeX :: amphiType -> int
homeX A = 2
homeX B = 4
homeX C = 6
homeX D = 8

homeRoom :: amphiType -> [position]
homeRoom t = [(homeX t, y) | y <- [1 .. 4]]

hallway, doors :: [position]
doors   = [(homeX t, 0) | t <- amphiTypes]
hallway = filter (not . member cmpposition doors) [(x, 0) | x <- [0 .. 10]]

reachedGoal :: burrow -> bool
reachedGoal
    = all finished . s_toList
      where
        finished (t, Finish, p) = True
        finished s              = False

|| the range a .. b, without a, and in increasing order
range' :: int -> int -> [int]
range' a b = [a + 1 .. b], if a < b
           = [b .. a - 1], otherwise

path == [position]

canMove :: m_map position amphipod -> path -> bool
canMove blocked = not . any (converse (m_member cmpposition) blocked)

|| generate the path from the amphipod position to another location
pathTo :: amphipod -> position -> path
pathTo (t, Start, (x1, y1)) (x2, y2) = [(x1, y) | y <- range' y1 y2] ++ [(x, y2) | x <- range' x1 x2]
pathTo (t, Hall,  (x1, y1)) (x2, y2) = [(x, y1) | x <- range' x1 x2] ++ [(x2, y) | y <- range' y1 y2]
pathTo _                    _        = error "pathTo: bad arguments"

|| generate list of possible moves and their costs for an amphipod
moves :: m_map position amphipod -> amphipod -> [(int, amphipod)]
moves blocked (t, Finish, p) = []
moves blocked a
    = (map (addCost a) . filter (canMove blocked . (a $pathTo)) . targets) a
      where
        addCost (t, Start, p1) p2 = (moveEnergy t * dist p1 p2, (t, Hall, p2))
        addCost (t, Hall, p1)  p2 = (moveEnergy t * dist p1 p2, (t, Finish, p2))
        addCost _              _  = error "addCost: bad arguments"

        targets (t, Start, p) = hallway
        targets (t, Hall, p)  = bestHomeRoom t
        targets _             = error "targets: bad argument"

        isOther t p
            = False,                  if isNothing ma
            = _ne cmpamphiType t' t, otherwise
              where
                ma         = m_lookup cmpposition p blocked
                (t', _, _) = fromJust ma

        bestHomeRoom t
            = [],             if any (isOther t) ps
            = lowestEmpty ps, otherwise
              where
                ps          = homeRoom t
                lowestEmpty = take 1 . filter (not . (converse (m_member cmpposition) blocked)) . reverse

allMoves :: burrow -> [(int, burrow)]
allMoves b
    = map (mapSnd (s_fromList cmpamphipod)) (go [] bl)
      where
        bl = s_toList b
        blocked = m_fromList cmpposition (zip2 (map amphiPos bl) bl)
        go as1 [] = []
        go as1 (a : as2)
            = map mkCostBurrow ms ++ go (a : as1) as2
              where
                ms                  = moves blocked a
                mkCostBurrow (c, a) = (c, a : as1 ++ as2)

estCost :: burrow -> int
estCost b
    = sum (map est bl)
      where
        bl = s_toList b
        est (t, Finish, p) = 0
        est (t, s, p) = moveEnergy t * (3 + dist p (hd (homeRoom t)))

solve :: burrow -> int
solve b
    = go (h_singleton (estCost b, (0, b))) s_empty
      where
        cmp st1 st2 = cmpint (fst st1) (fst st2)

        go q seen
            = go q1 seen, if s_member cmpburrow b seen
            = cost,                    if reachedGoal b
            = go q2 (s_insert cmpburrow b seen), otherwise
              where
                ((_, (cost, b)), q1) = fromJust $ h_viewMin cmp q
                bs                    = map (addCost cost) (allMoves b)
                q2                    = foldr (h_insert cmp) q1 bs
                addCost c (d, b)      = (estCost b + c + d, (c + d, b))

showBurrow :: burrow -> string
showBurrow b
    = intercalate "\n" (map showRow [0 .. 4]) ++ "\n\n"
      where
        bl = s_toList b
        bmap = m_fromList cmpposition (zip2 (map amphiPos bl) bl)
        showRow r
             = intercalate " " (map showCol [0 .. 10])
               where
                 showCol c
                     = ".",             if isNothing lu
                     = showamphiType t, otherwise
                       where
                         lu = m_lookup cmpposition (c, r) bmap
                         (t, _, _) = fromJust lu

day23 :: io ()
day23
    = io_mapM_ putStrLn [part1, part2]
      where
        b1      = [ (C,Start,(2,1)), (D,Start,(2,2)), (A,Start,(4,1)), (C,Start,(4,2))
                  , (B,Start,(6,1)), (A,Start,(6,2)), (D,Start,(8,1)), (B,Start,(8,2))
                  ]
        b2      = [ (C,Start,(2,1)), (D,Start,(2,2)), (D,Start,(2,3)), (D,Start,(2,4))
                  , (A,Start,(4,1)), (C,Start,(4,2)), (B,Start,(4,3)), (C,Start,(4,4))
                  , (B,Start,(6,1)), (B,Start,(6,2)), (A,Start,(6,3)), (A,Start,(6,4))
                  , (D,Start,(8,1)), (A,Start,(8,2)), (C,Start,(8,3)), (B,Start,(8,4))
                  ]
        part1  = (++) "part 1: " . showint . solve . s_fromList cmpamphipod $ b1
        part2  = (++) "part 2: " . showint . solve . s_fromList cmpamphipod $ b2
