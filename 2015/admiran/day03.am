|| day03.m


%export day03

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <mirandaExtensions>
%import <set>


location == (int, int)

moveDirection :: location -> char -> location
moveDirection (x, y) '^' = (x, y + 1)
moveDirection (x, y) 'v' = (x, y - 1)
moveDirection (x, y) '<' = (x - 1, y)
moveDirection (x, y) '>' = (x + 1, y)
moveDirection loc    _   = loc

deliver :: string -> s_set location
deliver
    = snd . foldl doMove (start, s_singleton start)
      where
        start = (0, 0)
        doMove (loc, locs) dir
            = (newLoc, s_insert cmplocation newLoc locs)
              where
                  newLoc = moveDirection loc dir

day03 :: io ()
day03
    = readFile "../inputs/day03.input" >>=. go
      where
        go input
            = io_mapM_ putStrLn [part1, part2]
              where
                (santa1, santa2) = mapBoth deliver . uninterleave $ input
                part1            = (++) "part 1: " . showint . s_size . deliver $ input
                part2            = (++) "part 2: " . showint . s_size $ s_union cmplocation santa1 santa2
