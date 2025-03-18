%export day02

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <maybe>
%import <mirandaExtensions>


move == (int, int, int)         || x, aim, y

addMove :: move -> move -> move
addMove (x1, a1, y1) (x2, a2, y2) = (x1 + x2, a1 + a2, y1 + y2 + a1 * x2)

moveProduct1 :: move -> int
moveProduct1 (x, a, y) = x * a

moveProduct2 :: move -> int
moveProduct2 (x, a, y) = x * y

readCommands :: string -> io [move]
readCommands fn
    = map (parse . words) <$>. lines <$>. readFile fn
      where
        parse [s1, s2] = go s1 (intval s2)
        parse ws       = error ("parse error: expected two words")
        go "forward" n = (n, 0,  0)
        go "up"      n = (0, -n, 0)
        go "down"    n = (0, n,  0)
        go c         n = error ("parse error: " ++ c)

day02 :: io ()
day02
    = readCommands "../inputs/day02.txt" >>=. go
      where
        go path
            = io_mapM_ putStrLn [part1, part2]
              where
                loc   = foldl addMove (0, 0, 0) path
                part1 = (++) "part 1: " . showint . moveProduct1 $ loc
                part2 = (++) "part 2: " . showint . moveProduct2 $ loc
