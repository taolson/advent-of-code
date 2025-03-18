|| day20.m


%export day20

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <mirandaExtensions>
%import <state> (>>=)/st_bind (>>)/st_right
%import <vector>

solve :: int -> int -> int -> int
solve input m lim
    = st_evalState (go 1) ()
      where
        d      = input $div m
        counts = v_unsafeThaw . v_rep d $ 0

        go i
            = st_mapM_ (step2 i) [1 .. min2 cmpint (d $div i) lim] >> v_unsafeRead counts (i - 1) >>= check
              where
                check n
                    = st_pure i,  if n >= input
                    = go (i + 1), otherwise

        step2 i j = v_unsafeModify counts (+ i * m) (j * i - 1)

day20 :: io ()
day20
    = io_mapM_ putStrLn [part1, part2]
      where
        input = 33_100_000
        part1 = (++) "part 1: " . showint $ solve input 10 input
        part2 = (++) "part 2: " . showint $ solve input 11 50
