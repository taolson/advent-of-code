|| day08.m -- Haunted Wasteland

%export day08

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <map>
%import <mirandaExtensions>


instruction == char
node        == string
network     == m_map node (node, node)

step :: network -> instruction -> node -> node
step net d loc
    = fst n, if d ==. 'L'
    = snd n, otherwise
      where
        n   = m_findWithDefault cmpstring err loc net
        err = error ("step: can't find " ++ loc)

findPeriod :: network -> [instruction] -> node -> int
findPeriod net moves loc
    = length . takeWhile notEnd . iterate loop $ (cycle moves, loc)
      where
        loop (d : ds, loc) = (ds, step net d loc)
        loop _             = error "empty moves"
        notEnd (_, loc)    = last loc ~=. 'Z'

totalPeriod :: network -> [instruction] -> [node] -> int
totalPeriod net moves locs
    = foldl1 lcm . map (findPeriod net moves) $ locs

readInput :: string -> io (string, network)
readInput fn
    = (go . splitWhen null . lines) <$>. readFile fn
      where
        go ([moves] : [net])
            = (moves, readNet net)
              where
                readNet = foldl ins m_empty . map (splitWhen (not . letter))

                ins m [k, l, r] = m_insert cmpstring k (l, r) m
                ins _ _         = error "readInput: parse error"
        go _ = error "parse error"

day08 :: io ()
day08
    = readInput "../inputs/day08.txt" >>=. go
      where
        go (moves, net)
            = io_mapM_ putStrLn [part1, part2]
              where
                starts = filter ((==. 'A') . last) . m_keys $ net
                part1  = ("part 1: " ++) . showint . findPeriod net moves  $ "AAA"
                part2  = ("part 2: " ++) . showint . totalPeriod net moves $ starts
