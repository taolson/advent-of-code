|| day02.m


%export day02

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <mirandaExtensions>


spread :: [int] -> int
spread lst = max cmpint lst - min cmpint lst

evenDivision :: [int] -> int
evenDivision xs
    = foldr check 0 [(n1, n2) | n1 : rest <- tails xs; n2 <- rest]
      where
        check (n1, n2) k
            = n1 $div n2, if n1 $mod n2 == 0
            = n2 $div n1, if n2 $mod n1 == 0
            = k,          otherwise

readSheet :: string -> io [[int]]
readSheet fn = map (map intval . words) <$>. lines <$>. readFile fn

day02 :: io ()
day02
    = readSheet "../inputs/day02.input" >>=. go
      where
        go sheet
            = io_mapM_ putStrLn [part1, part2]
              where
                part1 = (++) "part 1: " . showint . sum . map spread $ sheet
                part2 = (++) "part 2: " . showint . sum . map evenDivision $ sheet
