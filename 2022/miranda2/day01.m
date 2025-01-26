%export day01

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <maybe>
%import <mirandaExtensions>

readCargo :: string -> io [[int]]
readCargo fn
    = map (map intval) <$>. splitWhen null <$>. lines <$>. readFile fn

day01 :: io ()
day01
    = readCargo "../inputs/day01.txt" >>=. go
      where
        go elfCargo
            = io_mapM_ putStrLn [part1, part2]
              where
                elfCalories = sortBy (descending cmpint id) . map sum $ elfCargo
                part1       = (++) "part 1: " . showint . hd $ elfCalories
                part2       = (++) "part 2: " . showint . sum . take 3 $ elfCalories
