|| day08.m -- Resident Collinearity


%export day08

%import "adventLib"
%import <map>
%import <set>
%import <v2>            (|+|)/v2_add (|-|)/v2_sub

pos        == v2 int
antMap     == m_map char [pos]
antMapInfo == (antMap, pos)     || map, limit

makeAntennaInfo :: string -> antMapInfo
makeAntennaInfo s
    = (foldl ins m_empty ants, V2 nrows ncols)
      where
        rows  = lines s
        nrows = #rows
        ncols = #hd rows
        ants  = [(a, V2 r c) | (r, row) <- enumerate rows; (c, a) <- enumerate row; a ~=. '.']

        ins m (a, p) = m_insertWith cmpchar (++) a [p] m

onGrid :: pos -> pos -> bool
onGrid (V2 mr mc) (V2 r c) = 0 <= r < mr & 0 <= c < mc

allPairs :: [*] -> [(*, *)]
allPairs xs = [(a, b) | a : ys <- tails xs; b <- ys]

antiNodes :: pos -> pos -> pos -> [pos]
antiNodes lim a b = filter (onGrid lim) [a |+| d, b |-| d] where d = a |-| b

|| note: the way part 2 was specified, resonant nodes are *any* grid position
|| colinear with a and b, which would mean stepping by the GCD of the distance
|| between them, rather than just the distance.  However, all the input data had
|| a GCD equal to the distance, so we go the easy route...
resonantNodes :: pos -> pos -> pos -> [pos]
resonantNodes lim a b
    = allNodes (|+| d) a ++ allNodes (|-| d) b
      where
        d = a |-| b

        allNodes f p = iterate f p |> takeWhile (onGrid lim)

uniqueNodes :: antMapInfo -> (pos -> pos -> pos -> [pos]) -> int
uniqueNodes (amap, lim) genNodes
    = s_fromList cmppos ns |> s_size
      where
        ns = [n | xs <- m_elems amap; (a, b) <- allPairs xs; n <- genNodes lim a b]

day08 :: io ()
day08
    = readFile "../inputs/day08.txt" >>= makeAntennaInfo .> go
      where
        go ainfo
            = output [part1, part2]
              where
                part1 = uniqueNodes ainfo antiNodes     |> showint
                part2 = uniqueNodes ainfo resonantNodes |> showint
