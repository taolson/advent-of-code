|| day01.m


%export day01

%import <io>                    (>>=)/io_bind
%import <mirandaExtensions>

solve2, solve3 :: [int]  -> int
solve2 xs = hd [a * b     | a : bs <- tails xs; b <- bs; a + b == 2020]

solve3 xs = hd [a * b * c | a : bs <- tails xs;
                            b : cs <- tails bs;
                            a + b < 2020;       || early retry if we are already over our goal
                            c <- cs;
                            a + b + c == 2020]

day01 :: io ()
day01
    = readFile "../inputs/day01.txt" >>= (lines .> map intval .> go)
      where
        output as     = enumerate as |> io_mapM_ format
        format (i, s) = putStrLn $ "part " ++ showint i ++ ": " ++ s
        go expenses
            = output [part1, part2]
              where
                part1 = solve2 expenses |> showint
                part2 = solve3 expenses |> showint
