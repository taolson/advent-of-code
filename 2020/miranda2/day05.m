|| day05.m


%export day05

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <mirandaExtensions>


boardingPass == (int, int)

idToInt :: char -> string -> int
idToInt hi
    = foldl go 0
      where
        go n c = n * 2 + if' (c ==. hi) 1 0

readBoardingPass :: string -> boardingPass
readBoardingPass s
    = (row, seat)
      where
        (s1, s2) = splitAt 7 s
        row      = idToInt 'B' s1
        seat     = idToInt 'R' s2

seatID :: boardingPass -> int
seatID (row, seat) = row * 8 + seat

findMissing :: [int] -> int
findMissing
    = go . sortBy cmpint
      where
        go (a : b : ns)
             = a',          if a' ~= b
             = go (b : ns), otherwise
               where a' = a + 1

        go _ = error "findMissing: not found"

day05 :: io ()
day05
    = readFile "../inputs/day05.txt" >>=. go
      where
        go input
            = io_mapM_ putStrLn [part1, part2]
              where
                seatIDs = map (seatID . readBoardingPass) . lines $ input
                part1   = (++) "part 1: " . showint . max cmpint  $ seatIDs
                part2   = (++) "part 2: " . showint . findMissing $ seatIDs
