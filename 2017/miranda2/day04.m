|| day04.m


%export day04

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <mirandaExtensions>


validPassphrase :: [string] -> bool
validPassphrase words = length (nub cmpstring words) == #words

day04 :: io ()
day04
    = readFile "../inputs/day04.input" >>=. (go . map words . lines)
      where
        go pass
            = io_mapM_ putStrLn [part1, part2]
              where
                process = showint . length . filter validPassphrase
                part1   = (++) "part 1: " . process $ pass
                part2   = (++) "part 2: " . process . map sortWord $ pass
                          where sortWord = map (sortBy cmpchar)
