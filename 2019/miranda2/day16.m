%export day16

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <mirandaExtensions>


phase :: [int] -> [int]
phase elements
    = map step [1 .. #elements]
      where
        step m = (($mod 10) . abs . sum . zipWith (*) elements . drop 1 . cycle . concatMap (rep m)) [0, 1, 0, -1]

phase2 :: [int] -> int
phase2 signal
    = toInt fft
      where
        toInt ds = foldl acc 0 ds where acc s d = s * 10 + d
        sigSize  = #signal
        offset   = toInt . take 7 $ signal
        start    = offset $mod sigSize
        fullBlks = (sigSize * 10000 - offset) $div sigSize
        fullSig  = concat . replicate (fullBlks + 1) $ signal
        fft      = take 8 . drop start . hd . drop 100 . iterate (foldr pSum [0]) $ fullSig

        pSum d ds = (d + hd ds) $mod 10 : ds

day16 :: io ()
day16 = readFile "../inputs/day16.input" >>=. go
        where
          go input
              = io_mapM_ putStrLn [part1, part2]
                where
                  signal = map (intval . (: [])) . filter digit $ input
                  part1  = (++) "part 1: " . concatMap showint . take 8 . hd . drop 100 . iterate phase $ signal
                  part2  = (++) "part 2: " . showint . phase2 $ signal
