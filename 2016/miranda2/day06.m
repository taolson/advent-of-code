|| day06.m


%export day06

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <bag>
%import <mirandaExtensions>


charFreq == (char, int)

accumOccurrences :: string -> [b_bag char] -> [b_bag char]
accumOccurrences s bs
    = zipWith (b_insert cmpchar) s bs'
      where
        bs' = bs ++ rep (#s - #bs) b_empty

dcd :: ([charFreq] -> charFreq) -> [b_bag char] -> string
dcd f bs = map (fst . f . b_toList) $ bs

day06 :: io ()
day06
    = readFile "../inputs/day06.input" >>=. (go . lines)
      where
        go wds
            = io_mapM_ putStrLn [part1, part2]
              where
                bs    = foldr accumOccurrences [] $ wds
                part1 = (++) "part 1: " . dcd (maxBy cmpint snd) $ bs
                part2 = (++) "part 2: " . dcd (minBy cmpint snd) $ bs
