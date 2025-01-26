|| day13.m


%export day13

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <maybe>
%import <mirandaExtensions> -find
%import <parser> (<$>)/p_fmap (<*>)/p_apply (<*)/p_left (*>)/p_right (<|>)/p_alt


p_busID :: parser (maybe int)
p_busID = (Just <$> p_int) <|> (const Nothing <$> p_char 'x')

p_schedule :: parser (int, [maybe int])
p_schedule = p_liftA2 pair (p_int <* p_spaces) (p_someSepBy (p_char ',') p_busID) <* p_spaces

residualAfter :: int -> int -> int
residualAfter m n
    = n * mult - m
      where
        mult = (m $div n) + if' (m $mod n > 0) 1 0

find :: (int, int) -> (int, int) -> (int, int)
find (start, step) (n, m)
    = (start', step')
      where
        start'        = hd . dropWhile (noMatch n m) $ [start, start+step ..]
        step'         = step * n
        noMatch n m p = (p + m) $mod n ~= 0

readSchedule :: string -> io (int, [maybe int])
readSchedule fn
    = go <$>. parse p_schedule <$>. readFile fn
      where
        go (ms, ps) = fromMaybe (error (p_error ps)) ms

day13 :: io ()
day13
    = readSchedule "../inputs/day13.txt" >>=. go
      where
        addIdx Nothing   _   = Nothing
        addIdx (Just id) idx = Just (id, idx)

        go (depart, busIDs)
            = io_mapM_ putStrLn [part1, part2]
              where
                definedIDs = catMaybes busIDs
                definedIdx = catMaybes (zipWith addIdx busIDs [0 ..])
                bus        = minBy cmpint snd . zip2 definedIDs . map (residualAfter depart) $ definedIDs
                ts         = fst . foldl find (0, 1) $ definedIdx
                part1      = (++) "part 1: " . showint $ fst bus * snd bus
                part2      = (++) "part 2: " . showint $ ts
