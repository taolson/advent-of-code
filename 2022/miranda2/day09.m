%export day09

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <lens>
%import <maybe>
%import <mirandaExtensions>
%import <set>
%import <state>
%import <v2>


position == v2 int
step     == v2 int
rope     == [position]

follow :: position -> state position position
follow t h
    = (t', t')
      where
        delta = v2_sub h t
        step  = v2_signum delta
        move  = (v2_foldr (\/) False . v2_fmap ((> 1) . abs)) delta
        t'    = v2_add t step, if move
              = t,             otherwise

simulate :: int -> [step] -> s_set position
simulate n ss
    = snd (foldl doStep (rep n init, s_singleton init) ss)
      where
        init = V2 0 0
        doStep (r, ps) s
            = (h' : tr', s_insert cmpposition t ps)
              where
                h'       = v2_add (hd r) s
                (tr', t) = st_mapM follow (tl r) h'

readMoves :: string -> io [step]
readMoves fn
    = concatMap readMove <$>. lines <$>. readFile fn
      where
        readMove (d : ' ' : xs) = rep (intval xs) (moveDir d)
        readMove xs             = error "parse error"
        moveDir 'U' = V2 0    1
        moveDir 'D' = V2 0    (-1)
        moveDir 'L' = V2 (-1) 0
        moveDir 'R' = V2 1    0
        moveDir _   = error "moveDir: bad direction"

day09 :: io ()
day09
    = readMoves "../inputs/day09.txt" >>=. go
      where
        go moves
            = io_mapM_ putStrLn [part1, part2]
              where
                part1 = (++) "part 1: " . showint . s_size . simulate 2  $ moves
                part2 = (++) "part 2: " . showint . s_size . simulate 10 $ moves
        
