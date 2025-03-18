|| day06.m


%export day06

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import <vector>


memory == vector int

reallocateCycle :: memory -> memory
reallocateCycle v
    = update mx
      where
        len = v_length v
        mx  = fromJust . foldl findMaxWithIndex Nothing $ [0 .. len - 1]

        findMaxWithIndex mx i
            = go mx (v !! i)
              where
                go (Just (mn, mi)) n = Just (mn, mi), if n <= mn
                go mx              n = Just (n, i)

        update (mn, mi)
            = v // ((mi, 0) : [(i, (v !! i) + 1) | i <- map wrapIdx [1 .. mn]])
              where
                wrapIdx i = (mi + i) $mod len


findLoop :: m_map memory int -> memory -> int -> (int, int)
findLoop seen v cycle
    = fromMaybef (findLoop seen' v' cycle') (pair cycle) (m_lookup cmpmemory v seen)
      where
        seen'  = m_insert cmpmemory v cycle seen
        cycle' = cycle + 1
        v'     = reallocateCycle v

day06 :: io ()
day06
    = readFile "../inputs/day06.input" >>=. (go . v_fromList . map intval . words)
      where
        go memory
            = io_mapM_ putStrLn [part1, part2]
              where
                (cycle, seenCycle) = findLoop m_empty memory 0
                part1              = "part 1: " ++ showint cycle
                part2              = "part 2: " ++ showint (cycle - seenCycle)
