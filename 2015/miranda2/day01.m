|| day01.m


%export day01

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <mirandaExtensions>


parenVal :: char -> int
parenVal '(' = 1
parenVal ')' = -1
parenVal _   = 0

day01 :: io ()
day01
    = readFile "../inputs/day01.input" >>=. (go . map parenVal)
      where
        go input
            = io_mapM_ putStrLn [part1, part2]
              where
                part1 = (++) "part 1: " . showint . sum $ input
                part2 = (++) "part 2: " . showint . length . takeWhile (>= 0) . scanl (+) 0 $ input
