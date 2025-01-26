|| day17.m


%export day17

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <mirandaExtensions>
%import <zipper>

buffer ::= Buffer (zipper int) int int || array, size, position

next :: buffer -> (int, buffer)
next (Buffer z sz p)
    = (z_cursor z, Buffer (z_right z) sz p)

insertAfterSteps :: int -> buffer -> int -> buffer
insertAfterSteps steps (Buffer z sz p) elt
    = doIns z_left  (p - p1), if p1 <= p
    = doIns z_right (p1 - p), otherwise
      where
        p1  = (p + steps) $mod sz
        p2  = p1 + 1
        sz' = sz + 1

        doIns z_op n
            = z' $seq sz' $seq p2 $seq Buffer z' sz' p2
              where
                z' = z_insert elt . hd . drop n . iterate z_op $ z

doStep :: int -> int -> int -> int
doStep r p lim
    = go r p 1
      where
        go r p i
            = r,          if i  == lim
            = go i p' i', if p' == 1
            = go r p' i', otherwise
              where
                i' = i + 1
                p' = (p + 386) $mod i + 1

day17 :: io ()
day17
    = io_mapM_ putStrLn [part1, part2]
      where
        b     = foldl (insertAfterSteps 386) (Buffer (z_singleton 0) 1 0) [1 .. 2017]
        part1 = (++) "part 1: " . showint . fst . next $ b
        part2 = (++) "part 2: " . showint $ doStep 0 0 50_000_000
