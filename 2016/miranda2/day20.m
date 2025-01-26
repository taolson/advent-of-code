|| day20.m


%export day20

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <maybe>
%import <mirandaExtensions>
%import <parser> (*>)/p_right


interval == (int, int)

merge :: interval -> interval -> maybe interval
merge (lo1, hi1) (lo2, hi2)
    = Just (lo, hi), if hi1 + 1 >= lo2 & hi2 + 1 >= lo1
    = Nothing,       otherwise
      where
        lo = min2 cmpint lo1 lo2
        hi = max2 cmpint hi1 hi2

blackList :: [interval] -> [interval]
blackList [] = []
blackList rs
    = reverse (last : merged')
      where
        (last, merged') = foldl mergeRanges (hd rs, []) $ tl rs
        mergeRanges (prev, black) range
            = fromMaybef (range, prev : black) ($pair black) (merge prev range)

lowestNotBlocked :: [interval] -> int
lowestNotBlocked [] = error "lowestNotBlocked: empty blackList"
lowestNotBlocked ((lo, hi) : rest)
    = lo - 1, if lo > 0
    = hi + 1, otherwise

allowed :: [interval] -> int
allowed rs
    = snd . foldl countBetween (belowLowestIP, 0) $ rs ++ [(aboveHighestIP, aboveHighestIP)]
      where
        belowLowestIP  = -1
        aboveHighestIP = 2 ^ 32

        countBetween (prevHi, count) (lo, hi)
            = (hi, count + (lo - prevHi - 1))

p_range :: parser interval
p_range = p_liftA2 pair p_posint (p_char '-' *> p_posint)

readRanges :: string -> io [interval]
readRanges fn
    = go <$>. parse (p_someSepBy p_spaces p_range) <$>. readFile fn
      where
        go (mrs, ps) = fromMaybe (error (p_error ps)) mrs

day20 :: io ()
day20
    = readRanges "../inputs/day20.input" >>=. (go . blackList . sortBy cmpinterval)
      where
        go bl
            = io_mapM_ putStrLn [part1, part2]
              where
                part1 = (++) "part 1: " . showint . lowestNotBlocked $ bl
                part2 = (++) "part 2: " . showint . allowed $ bl

