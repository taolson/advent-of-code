%export day06

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <maybe>
%import <mirandaExtensions>

slidingWindow :: int -> [*] -> [[*]]
slidingWindow n = takeWhile ((== n) . length) . map (take n) . tails

indexOfPacket :: int -> [char] -> int
indexOfPacket n
    = (+ n) . length . takeWhile ((~= n) . length . nub cmpchar) . slidingWindow n

day06
    = readFile "../inputs/day06.txt" >>=. go
      where
        go signal
            = io_mapM_ putStrLn [part1, part2]
              where
                part1  = (++) "part 1: " . showint . indexOfPacket 4  $ signal
                part2  = (++) "part 2: " . showint . indexOfPacket 14 $ signal
