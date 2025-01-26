%export day05


%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <map>
%import <maybe>
%import <mirandaExtensions>


crate    == char
stack    == [crate]
stackMap == m_map int stack
move     == (int, int, int)

|| collect the stackInfo in row order, adding the crate to a stack numbered by it's
|| column position in the row.  At the end, the actual stack indicies (as characters)
|| are at the top of each stack, with the crates below them in reverse order
makeStackInfo :: [string] -> stackMap
makeStackInfo rows
    = foldl insCrate m_empty crateLocs
      where
        crateLocs         = concatMap (filter isInfo . zip2 [0 ..]) rows
        isInfo (i, c)     = letter c \/ digit c
        insCrate m (i, c) = m_insertWith cmpint push i [c] m
        push [c] cs       = c : cs
        push xs cs        = error "makeStackInfo: bad crate value"

|| take the stacks in the stackInfoMap, and use the top entry of each stack
|| as the stack number, adding the reverse of the crates below it
makeStackMap :: stackMap -> stackMap
makeStackMap
    = foldl insStack m_empty . m_toList
      where
        insStack m (x, ci : cs) = m_insert cmpint (digitVal ci) (reverse cs) m
        insStack m (x, cs)      = error "makeStackMap: bad insStack value"

makeMove :: string -> move
makeMove 
    = tup3 . map intval . filter isInt . words
      where
        isInt (c : cs) = digit c
        isInt _        = error "isInt: empty string"
        tup3 [a, b, c]    = (a, b, c)
        tup3 _            = error ("tup3: bad list")

mover == string -> string -> int -> (string, string)

moveSingle, moveMany :: mover
moveSingle fs ts n
    = iterate move (fs, ts) ! n
      where
        move (c : s1, s2) = (s1, c : s2)
        move _            = error "no data for move"

moveMany fs ts n
    = (fs', ts')
      where
        (cs, fs') = splitAt n fs
        ts'       = cs ++ ts

process :: mover -> stackMap -> move -> stackMap
process f sm (n, fi, ti)
    = sm2
      where
        fs         = fromJust (m_lookup cmpint fi sm)
        ts         = fromJust (m_lookup cmpint ti sm)
        (fs', ts') = f fs ts n
        sm1        = m_insert cmpint fi fs' sm
        sm2        = m_insert cmpint ti ts' sm1

readInput :: string -> io (stackMap, [move])
readInput fn
    = go <$>. break null <$>. lines <$>. readFile fn
      where
        go (crateInfo, moveInfo)
            = (stacks, moves)
              where
                stackInfo = makeStackInfo crateInfo
                stacks    = makeStackMap stackInfo
                moves     = map makeMove (tl moveInfo)

day05 :: io ()
day05
    = readInput "../inputs/day05.txt" >>=. go
      where
        go (sm, moves)
            = io_mapM_ putStrLn [part1, part2]
              where
                part1 = (++) "part 1: " . map hd . m_elems . foldl (process moveSingle) sm $ moves
                part2 = (++) "part 2: " . map hd . m_elems . foldl (process moveMany) sm   $ moves
