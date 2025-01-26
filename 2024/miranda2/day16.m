|| day16.m -- Reindeer Maze


%export day16

%import "adventLib"
%import <heap>
%import <map>
%import <maybe>
%import <set>
%import <v2>

pos == v2 int
dir ::= N | E | S | W

dirs :: [dir]
dirs = [N, E, S, W]

reindeer == (pos, dir)

move, turnRight, turnLeft :: reindeer -> reindeer
turnRight (p, d) = (p, d') where d' = case d of N -> E; E -> S; S -> W; W -> N
turnLeft  (p, d) = (p, d') where d' = case d of N -> W; E -> N; S -> E; W -> S

move (V2 r c, d)
    = (p, d)
      where
        p = case d of
              N -> V2 (r - 1) c
              E -> V2 r (c + 1)
              S -> V2 (r + 1) c
              W -> V2 r (c - 1)

walls == s_set pos
maze  == (walls, pos, pos)      || walls, start, end

makeMaze :: string -> maze
makeMaze s
    = foldl ins (s_empty, v2_pure 0, v2_pure 0) info
      where
        info = [(V2 r c, x) | (r, row) <- s |> lines |> enumerate; (c, x) <- enumerate row]

        ins (w, s, e) (p, x)
            = go x
              where
                go '#' = (s_insert cmppos p w, s, e)
                go 'S' = (w, p, e)
                go 'E' = (w, s, p)
                go _   = (w, s, e)

|| solve using a breadth-first search on the maze, but maintain a global seen map of (pos, dir) -> cost
|| instead of a global seen set of (pos, dir), and follow all paths that are <= the minimum cost at
|| a seen entry; that way we follow all possible solution paths instead of early termination if we
|| already visited a reindeer pos, dir, to allow us to solve part 2
solve :: maze -> (int, s_set pos)
solve (walls, start, end)
    = go m_empty $ h_singleton (0, (start, E), [])
      where
        cmpcost (c1, _, _) (c2, _, _) = cmpint c1 c2    || comparison function for inserting into heap

        go seen q
            = case h_viewMin cmpcost q of
                Nothing -> error "no solution found"
                Just v  -> check seen v

        check seen ((cst, r, pth), q1)
            = getRest cst r pth' q1, if _eq cmppos p end                                        || found a min solution; get rest of equal solutions
            = go seen  q1,           if m_lookup cmpreindeer r seen |> fromMaybef False (< cst) || have visited this (pos, dir) earlier; drop this path
            = go seen' q2,           otherwise                                                  || add expansion from here to the queue and continue
              where
                p     = fst r
                seen' = m_insert cmpreindeer r cst seen
                pth'  = p : pth
                q2    = foldr (h_insert cmpcost) q1 $ tryMove ++ tryTurn

                tryTurn = [(cst + 1000, r', pth') | r' <- [turnRight r, turnLeft r]]                    || turns cost 1000 each
                tryMove = [(cst + 1, r', pth')    | r' <- [move r]; ~s_member cmppos (fst r') walls]    || moves cost 1 each
                
        || found a min solution; get the rest that have the same cost to collect all the path positions
        getRest cst r pth q
            = Just ((cst, r, pth), q) |> iterate getNext |> takeWhile sameCost |> filter isSolution |> foldl mergePath s_empty |> pair cst
              where
                getNext (Just (_, q)) = h_viewMin cmpcost q
                getNext _ = Nothing

                sameCost (Just ((c, _, _), _)) = c == cst
                sameCost _ = False

                isSolution (Just ((_, (p, _), _), _)) = _eq cmppos p end
                isSolution _ = False

                mergePath s (Just ((_, _, pth), _)) = foldr (s_insert cmppos) s pth
                mergePath s _ = s

day16 :: io ()
day16
    = readFile "../inputs/day16.txt" >>= makeMaze .> go
      where
        go maze
            = output [part1, part2]
              where
                (cst, posSet) = solve maze
                part1         = cst |> showint
                part2         = s_size posSet |> showint
