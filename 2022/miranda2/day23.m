%export day23

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <lens>
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import <set>
%import <v2>


location  == v2 int
direction == v2 int
grid      == s_set location
scanSt    == [bool]
check     == (scanSt, direction)
checkSt   == [check]

scanGrid :: grid -> location -> scanSt
scanGrid g loc = [~(r == c == 0) & s_member cmplocation (v2_add loc (V2 r c)) g | r, c <- [-1 .. 1]]

checkN, checkS, checkE, checkW :: check
checkN = (map letter "XXX......", V2 (-1) 0)
checkS = (map letter "......XXX", V2 1    0)
checkE = (map letter "..X..X..X", V2 0    1)
checkW = (map letter "X..X..X..", V2 0 (-1))

proposals == m_map location [location]  || key = proposed move, values = list of elf locations proposing to move to the key

proposePhase :: grid -> checkSt -> proposals
proposePhase g cs
    = foldl mkProp m_empty (s_toList g)
      where
        mkProp pm loc
            = m_insertWith cmplocation (++) loc  [loc] pm, if ~or sc \/ isNothing prop
            = m_insertWith cmplocation (++) loc' [loc] pm, otherwise
              where
                sc   = scanGrid g loc
                prop = find (not . or . zipWith (&) sc . fst) cs    || find first direction that is valid
                loc' = (v2_add loc . snd . fromJust) prop

simulate :: int -> grid ->  checkSt -> (int, grid)
simulate limit
    = go 0
      where
        go n g cs
            = (n + 1, g),        if n == limit \/ null move
            = go (n + 1) g' cs2, otherwise
              where
                (cs1, _ : cs2) = splitAt 4 cs
                prop           = proposePhase g cs1
                (move, stay)   = partition canMove (m_toList prop)
                g'             = foldl insMove (foldl insStay s_empty stay) move

                canMove (x, [x'])   = _ne cmplocation x x'
                canMove p           = False
                insMove s (loc, xs) = s_insert cmplocation loc s
                insStay s (loc, xs) = foldl (converse (s_insert cmplocation)) s xs
                
emptyTiles :: grid -> int
emptyTiles g
    = area - elves
      where
        elves    = s_size g
        first    = s_first g
        (b1, b2) = foldl bboxLoc (first, first) (s_toList g)
        area     = v2_product (V2 1 1 $v2_add b2 $v2_sub b1)

        bboxLoc (lo, hi) loc = (v2_min cmpint lo loc, v2_max cmpint hi loc)

readGrid :: string -> io grid
readGrid fn
    = go <$>. lines <$>. readFile fn
      where
        go input
            = s_fromList cmplocation [V2 r c | (r, row) <- enumerate input; (c, '#') <- enumerate row]

day23 :: io ()
day23
    = readGrid "../inputs/day23.txt" >>=. go
      where
        go g
            = io_mapM_ putStrLn [part1, part2]
              where
                cs    = cycle [checkN, checkS, checkW, checkE]
                part1 = (++) "part 1: " . showint . emptyTiles . snd . simulate 10 g $ cs
                part2 = (++) "part 2: " . showint . fst . simulate (-1) g $ cs
