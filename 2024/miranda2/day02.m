|| day02.m -- Red Nosed Reports


%export day02

%import "adventLib"


report == [int]

deltas :: report -> [int]
deltas [] = []
deltas xs = zipWith (-) xs $ tl xs

minMax :: [int] -> (int, int)
minMax [] = error "minMax []"
minMax (x : xs)
    = foldl go (x, x) xs
      where
        go (lo, hi) x = (min2 cmpint lo x, max2 cmpint hi x)

|| is a report increasing by 1..3 or decreasing by 1..3 everywhere?
isSafe :: report -> bool
isSafe
    = deltas .> minMax .> check
      where
        check (lo, hi) = 1 <= lo <= hi <= 3 \/ -1 >= hi >= lo >= -3

|| all reports with a single element removed, plus the original report
dampen :: report -> [report]
dampen []       = [[]]
dampen (x : xs) = xs : map (x :) (dampen xs)

day02 :: io ()
day02
    = readFile "../inputs/day02.txt" >>= lines .> map (words .> map intval) .> go
      where
        go reports
            = output [part1, part2]
              where
                part1 = filter isSafe reports |> length |> showint
                part2 = filter (any isSafe) (map dampen reports) |> length |> showint
