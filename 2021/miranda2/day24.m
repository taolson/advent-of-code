|| solved by hand by examining the supplied program, and determining
|| the constraints on the digits
||
|| code below was used to run various inputs on the program

%export day24

%import <io>


|| solved by hand examination of binary

day24 :: io ()
day24
    = io_mapM_ putStrLn [part1, part2]
      where
        part1  = "part 1: " ++ "99893999291967"
        part2  = "part 2: " ++ "34171911181211"
