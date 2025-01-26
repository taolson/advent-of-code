%export day01

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <maybe>
%import <mirandaExtensions>


deltas :: [int] -> [int]
deltas [] = []
deltas xs = zipWith (-) (tl xs) xs

windowSum :: int -> [int] -> [int]
windowSum n = map sum . transpose . take n . tails

incrs :: [int] -> int
incrs = length . filter (> 0) . deltas

readDepths :: string -> io [int]
readDepths fn = map intval <$>. lines <$>. readFile fn

day01 :: io ()
day01
    = readDepths "../inputs/day01.txt" >>=. go
      where
        go depths
            = io_mapM_ putStrLn [part1, part2]
              where
                part1 = (++) "part 1: " . showint . incrs $ depths
                part2 = (++) "part 2: " . showint . incrs . windowSum 3 $ depths
