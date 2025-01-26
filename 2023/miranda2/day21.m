|| day21.m -- Step Counter


%export day21

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <dequeue>
%import <lens>
%import <maybe>
%import <mirandaExtensions>
%import <set>


loc    == (int, int)
garden == (s_set loc, loc)      || rocks, size

|| lenses for garden
g_rocks = lensFst
g_size  = lensSnd

|| generate a location modulo a size limit
wrapLoc :: loc -> loc -> loc
wrapLoc (r, c) (nr, nc) = (r $mod nr, c $mod nc)

expand :: (loc -> bool) -> loc -> [loc]
expand p (r, c) = filter p [(r + dr, c + dc) | (dr, dc) <- [(-1, 0), (1, 0), (0, -1), (0, 1)]]

|| find the number of locations reachable from a starting location in the given number of steps
reachable :: garden -> loc -> int -> int
reachable (rs, sz) start steps
    = go q s_empty s_empty
      where
        q = dq_addR (0, start) dq_empty

        parity = even,            if even steps
               = odd,             otherwise

        go q seen reach
            = s_size reach,       if dq_null q
            = go q1 seen  reach,  if n > steps \/ s_member cmploc loc seen
            = go q2 seen' reach', otherwise
              where
                ((n, loc), q1) = fromJust $ dq_viewL q
                seen'          = s_insert cmploc loc seen
                q2             = foldl insq q1 . map (pair (n + 1)) . expand notRock $ loc

                reach'         = s_insert cmploc loc reach, if parity n
                               = reach,                     otherwise

                insq q x       = dq_addR x q
                notRock loc    = ~s_member cmploc (wrapLoc loc sz) rs

|| part 2:  Looking at the actual input, I see that it is set up with S in the very center, with an unblocked
|| straight line to N, S, E, and W edges, and there are no rocks on any of the edges.  So At time N, we simultaneously
|| hit the next iteration of N, S, E, and W from the opposite edge of the direction of travel, in the center.  So I think
|| all that needs to be done is figure out the total filled for 1 iteration of the garden (limit travel to 0,0 - N, N,
|| then figure out the modulo of the total steps with the half-size of the garden, and traverse that many from the N, S
|| E, and W edges to get the final count
reachableInf :: garden -> loc -> int -> int
reachableInf g start steps
    = a * fullPlots * fullPlots + b * fullPlots + c
      where
        sz         = fst . view g_size $ g
        remSteps   = steps $mod sz
        fullPlots  = steps $div sz
        seen0      = reachable g start remSteps
        seen1      = reachable g start $ remSteps + sz
        seen2      = reachable g start $ remSteps + 2 * sz

        || calculate quadratic from the seen values
        a = (seen2 + c - 2 * seen1) $div 2
        b = seen1 - c - a
        c = seen0

readGarden :: string -> io (garden, loc)
readGarden fn
    = (go . lines) <$>. readFile fn
      where
        go rows
            = ((s_fromList cmploc rocks, size), hd starts)
              where
              size = (#rows, # (hd rows))
              rs   = [((r, c), ch) | (r, row) <- enumerate rows; (c, ch) <- enumerate row; ch ~=. '.']
              (rocks, starts) = mapBoth (map fst) . partition ((==. '#') . snd) $ rs
        
day21 :: io ()
day21
    = readGarden "../inputs/day21.txt" >>=. go
      where
        go (g, st)
            = io_mapM_ putStrLn [part1, part2]
              where
                part1 = (++) "part 1: " . showint . reachable g st $ 64
                part2 = (++) "part 2: " . showint . reachableInf g st $ 26501365
