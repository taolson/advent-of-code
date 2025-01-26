|| day06.m -- Guard Gallivant


%export day06

%import "adventLib"
%import <map>
%import <maybe>
%import <set>
%import <v2>


pos      == v2 int
obsSet   == s_set pos
gridInfo == (obsSet, pos, pos) || obsSet, grid limits, guard pos

makeGridInfo :: string -> gridInfo
makeGridInfo input
    = (os, V2 nrows ncols, gp)
      where
        rows  = lines input
        nrows = #rows
        ncols = #hd rows
        (os, gp) = foldl addInfo (s_empty, V2 0 0) [(V2 r c, x) | (r, row) <- enumerate rows; (c, x) <- enumerate row; x ~=. '.']

        addInfo (s, gp) (p, '#') = (s_insert cmppos p s, gp)
        addInfo (s, gp) (p, '^') = (s, p)
        addInfo sgp     _        = sgp

guardPos :: gridInfo -> pos
guardPos (_, _, p) = p


dir ::= N | E | W | S

step :: dir -> pos -> pos
step d (V2 r c)
    = case d of
        N -> V2 (r - 1) c
        E -> V2 r (c + 1)
        S -> V2 (r + 1) c
        W -> V2 r (c - 1)

turnRight :: dir -> dir
turnRight d
    = case d of
        N -> E
        E -> S
        S -> W
        W -> N

posdir  == (pos, dir)
path    == [posdir]
jumpMap == m_map posdir posdir

|| can the guard possibly hit a given obstruction?
canHit :: posdir -> pos -> bool
canHit (V2 r1 c1, d) (V2 r2 c2)
    = case d of
        N -> c1 == c2 & r1 > r2
        E -> r1 == r2 & c1 < c2
        S -> c1 == c2 & r1 < r2
        W -> r1 == r2 & c1 > c2

|| generate the guard's path until they leave the area
guardPath :: gridInfo -> path
guardPath (os, (V2 mr mc), gp)
    = iterate move (gp, N) |> takeWhile (fst .> onGrid)
      where
        onGrid (V2 r c) = 0 <= r < mr & 0 <= c < mc

        move (p, d)
            = (p, d'), if s_member cmppos p' os        || hit an obstruction; turn right (Note: don't step, because there could be another obstruction)
            = (p', d), otherwise                       || continue in same direction
              where
                p' = step d p
                d' = turnRight d


|| build a map that can quickly jump from one obstruction to the next,
|| bypassing all the stepping in between
makeJumpMap :: path -> jumpMap
makeJumpMap [] = m_empty
makeJumpMap (x : xs)
    = go m_empty x xs
      where
        go jm a [b] = m_insert cmpposdir a b jm         || final step to off-grid
        go jm a (b : xs)
            = check a b
              where
                check (p, d) (p', d')
                    = go jm  a xs,  if _eq cmpdir d d'  || same direction; keep checking
                    = go jm' b xs, otherwise            || hit an obstruction; insert into jumpMap
                      where
                        jm' = m_insert cmpposdir a b jm

        go _ _ _ = undef        || added to remove compiler warning

|| find all unique positions in the path
uniquePos :: path -> [pos]
uniquePos = map fst .> s_fromList cmppos .> s_toList

|| place an obstacle into the grid and check if it creates a loop in the path
|| by looking for a duplicate position with the same direction in the path
|| originally written to build a set of visited posdirs, but it is much faster
|| to use the tortise/hare algorithm to detect a loop (30s -> 8s)
|| also uses jumpMap to quickly skip stepping in the same direction
makesLoop :: gridInfo -> jumpMap -> pos -> bool
makesLoop (os, V2 mr mc, gp) jm op
    = quickPath (gp, N) |> check
      where
        onGrid (V2 r c) = 0 <= r < mr & 0 <= c < mc

        || quickly jump from obstruction to obstruction
        quickPath pd
            = quickPath $ fromJust mpd, if ~canHit pd op & isJust mpd
            = stepPath pd,              otherwise
              where
                mpd = m_lookup cmpposdir pd jm

        || step along path if we canHit new obstruction or are on a new path not cached in the jumpMap
        stepPath (p, d)
            = [],                                  if ~onGrid p
            = (p, d) : quickPath (p, turnRight d), if _eq cmppos p' op \/ s_member cmppos p' os
            = (p, d) : stepPath (p', d),           otherwise
              where
                p' = step d p

        || use tortise/hare algorithm: have one pointer run twice as fast along the generated
        || path, and if they ever wind up at the same posdir, it's a loop
        check pth
            = go pth (tl pth), otherwise
              where
                go (pd : pds) (pd' : _ : pds')
                    = True,        if _eq cmpposdir pd pd'
                    = go pds pds', otherwise

                go _ _ = False    || path ran out, so we must have exited the grid

day06 :: io ()
day06
    = readFile "../inputs/day06.txt" >>= makeGridInfo .> go
      where
        go gi
            = output [part1, part2]
              where
                path    = guardPath gi
                jm      = makeJumpMap path
                upos    = path |> uniquePos
                upos'   = filter (_ne cmppos gp) upos where gp = guardPos gi
                part1   = length upos |> showint
                part2   = count (makesLoop gi jm) upos' |> showint
