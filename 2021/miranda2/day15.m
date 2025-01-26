%export day15

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <heap>
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import <set>

point == (int, int)

grid  ::= Grid (m_map point int) int int        || gridmap, maxX, maxY

adjacent :: bool -> grid -> point -> [point]
adjacent part2 (Grid maze maxX maxY) (x, y)
    = filter inBounds [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
      where
        inBounds (x, y) = x >= 0 & x < mx & y >= 0 & y < my
        mx              = 5 * maxX, if part2
                        = maxX,     otherwise
        my              = 5 * maxY, if part2
                        = maxY,     otherwise

riskLevel :: grid -> point -> int
riskLevel (Grid maze maxX maxY) (x, y)
    = reduce r
      where
        x'       = x $mod maxX
        y'       = y $mod maxY
        dx       = x $div maxX
        dy       = y $div maxY
        r        = fromJust (m_lookup cmppoint (x', y') maze) + dx + dy
        reduce n = n,              if n < 10
                 = reduce (n - 9), otherwise


findMinRisk :: bool -> grid -> int
findMinRisk part2 g
    = go (h_singleton (0, (0, 0))) s_empty
      where
        Grid _ maxX maxY = g
        mx               = maxX * 5 - 1, if part2
                         = maxX - 1,     otherwise
        my               = maxY * 5 - 1, if part2
                         = maxY - 1,     otherwise
        finish           = (mx, my)        
        cmp p1 p2        = cmpint (fst p1) (fst p2)

        go q s
            = error "empty queue", if h_null q
            = go q1 s,             if s_member cmppoint p s
            = r,                   if _eq cmppoint p finish
            = go q2 s',            otherwise
              where
                ((r, p), q1) = fromJust $ h_viewMin cmp q
                s'           = s_insert cmppoint p s
                adj          = adjacent part2 g p
                adjRisks     = map addRisk adj
                addRisk p    = (r + riskLevel g p, p)
                q2           = foldr (h_insert cmp) q1 adjRisks


readInput :: string -> io grid
readInput fn
    = go <$>. lines <$>. readFile fn
      where
        go rows
            = Grid (m_fromList cmppoint pts) maxX maxY
              where
                maxX = #rows
                maxY = #(hd rows)
                pts  = [((x, y), digitVal d) | (y, row) <- enumerate rows; (x, d) <- enumerate row]

day15 :: io ()
day15
    = readInput "../inputs/day15.txt" >>=. go
      where
        go g
            = io_mapM_ putStrLn [part1, part2]
              where
                part1 = (++) "part 1: " . showint . findMinRisk False $ g
                part2 = (++) "part 2: " . showint . findMinRisk True  $ g
