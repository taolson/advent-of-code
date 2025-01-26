|| day01.m


%export day01

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <mirandaExtensions>
%import <set>

parseNumber :: string -> int
parseNumber [] = 0
parseNumber s
    = parseNumber cs, if c ==. '+'
    = intval s,       otherwise
      where
        c : cs = s

findCycle :: [int] -> int
findCycle input
    = go (cycle input) s_empty 0
      where
        go [] _ _ = error "part2: go []"
        go (n : ns) seen freq
            = freq,                                         if s_member cmpint freq seen
            = go ns (s_insert cmpint freq seen) (freq + n), otherwise
    
day01 :: io ()
day01
    = readFile "../inputs/day01.input" >>=. (go . map parseNumber . lines)
      where
        go input
            = io_mapM_ putStrLn [part1, part2]
              where
                part1 = (++) "part 1: " . showint . sum       $ input
                part2 = (++) "part 2: " . showint . findCycle $ input
