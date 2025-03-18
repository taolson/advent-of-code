%export day08

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <lens>
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import <set>

location  == (int, int)

dist :: location -> location -> int
dist (a, b) (c, d) = abs (a - c) + abs (b - d)

visible == s_set location             || set of trees visible from the perimeter
scenic  == m_map location [int]       || map of trees to a list of their scenic distances in each direction
tree    == (location, int)            || a location and a tree height
scanSt  == (visible, scenic)

|| scan a list of trees, tallying their visibility from the edge, along with their
|| scenic distance in this direction
scanOneDir :: scanSt -> [tree] -> scanSt
scanOneDir st xs
    = view lensTup3_0 (foldl step (st, -1, initHm) xs)
      where
        initHm = rep 10 (fst (hd xs))
        step ((vis, scn), mh, hm) (loc, h)
            = ((vis', scn'), mh', hm')
              where
                vis'  = s_insert cmplocation loc vis, if h > mh
                      = vis,              otherwise
                scn'  = m_insertWith cmplocation (++) loc [dist loc (hm ! h)] scn
                mh'   = max2 cmpint h mh
                hm'   = rep (h + 1) loc ++ drop (h + 1) hm

findVisibility :: [[tree]] -> scanSt
findVisibility rows
    = foldl (foldl scanOneDir) (s_empty, m_empty) [rows, map reverse rows, cols, map reverse cols]
      where
        cols = transpose rows

readTrees :: string -> io [[tree]]
readTrees fn
    = map mkRow <$>. zip2 [0 ..] <$>. lines <$>. readFile fn
      where
        mkRow (r, cs)  = map (mkCol r) (zip2 [0 ..] (map digitVal cs))
        mkCol r (c, h) = ((r, c), h)

day08 :: io ()
day08
    = readTrees "../inputs/day08.txt" >>=. go
      where
        go trees
            = io_mapM_ putStrLn [part1, part2]
              where
                (vis, scn) = findVisibility trees
                scnScores  = map product (m_elems scn)
                part1      = (++) "part 1: " . showint . s_size $ vis
                part2      = (++) "part 2: " . showint . max cmpint $ scnScores
