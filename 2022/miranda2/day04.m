%export day04

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <maybe>
%import <mirandaExtensions>


section     == (int, int)
sectionPair == (section, section)

overlaps :: sectionPair -> bool
overlaps ((a, b), (c, d)) = a <= d & b >= c

encloses :: sectionPair -> bool
encloses ((a, b), (c, d))
    = a <= c & b >= d \/
      a >= c & b <= d

readAssigns :: string -> io [sectionPair]
readAssigns fn
    = map (mkPair . map readSection . split ',') <$>. lines <$>. readFile fn
      where
        readSection = mkPair . map intval . split '-'
        mkPair [a, b] = (a, b)
        mkPair xs     = error "mkPair: bad value"

day04 :: io ()
day04
    = readAssigns "../inputs/day04.txt" >>=. go
      where
        go assigns
            = io_mapM_ putStrLn [part1, part2] 
              where
                part1 = (++) "part 1: " . showint . length . filter encloses $ assigns
                part2 = (++) "part 2: " . showint . length . filter overlaps $ assigns
