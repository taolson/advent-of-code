|| day01.m


%export day01

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <mirandaExtensions>


matchesWithDistance :: int -> string -> int
matchesWithDistance n s
    = foldr ((+) . match) 0 $ zip2 s rotated
      where
        rotated      = cycle s |> drop n
        match (a, b) = if' (a ==. b) (digitVal a) 0

day01 :: io ()
day01
    = readFile "../inputs/day01.input" >>=. go
      where
        go input
            = io_mapM_ putStrLn [part1, part2]
              where
                part1 = (++) "part 1: " . showint . matchesWithDistance 1 $ input
                part2 = (++) "part 2: " . showint . matchesWithDistance (#input $div 2) $ input
