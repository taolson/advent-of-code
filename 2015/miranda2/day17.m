|| day17.m


%export day17

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <mirandaExtensions>


canFillExactly :: int -> [int] -> [[int]]
canFillExactly 0 [] = [[]]
canFillExactly n [] = []
canFillExactly n (c : cs)
    = fs,       if n < c
    = fs ++ gs, otherwise
      where
        fs = canFillExactly n cs
        gs = map (c :) $ canFillExactly (n - c) cs

numOfMinLength :: [[*]] -> int
numOfMinLength xs
    = #filter (== mn) lens
      where
        (mn, lens) = mapAccumL go (#hd xs) xs

        go m x = (min2 cmpint m n, n) where n = #x
            
day17 :: io ()
day17
    = readFile "../inputs/day17.input" >>=. (go . map intval . lines)
      where
        go cs
            = io_mapM_ putStrLn [part1, part2]
              where
                fs = canFillExactly 150 cs
                part1 = (++) "part 1: " . showint . length $ fs
                part2 = (++) "part 1: " . showint . numOfMinLength $ fs
