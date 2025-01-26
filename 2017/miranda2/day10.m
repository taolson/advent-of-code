|| day10.m


%export day10

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <maybe>
%import <mirandaExtensions>
%import <parser> 
%import "knotHash"

readIntList :: string -> [int]
readIntList s
    = fromMaybe err mlst
      where
        (mlst, ps) = parse p_intlist s
        err        = error (p_error ps)

day10 :: io ()
day10
    = io_mapM_ putStrLn [part1, part2]
      where
        kh      = makeKnotHash 256
        input   = "94,84,0,79,2,27,81,1,123,93,218,23,103,255,254,243"
        kh'     = foldl hashRound kh $ readIntList input
        b0      = hashIndex kh' 0 
        b1      = hashIndex kh' 1 
        part1 = (++) "part 1: " . showint $ b0 * b1
        part2 = (++) "part 2: " . hexString . hash kh $ input
