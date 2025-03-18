|| day01.m -- Historian Hysteria


%export day01

%import "adventLib"
%import <bag>

day01 :: io ()
day01
    = readFile "../inputs/day01.txt" >>= words .> map intval .> uninterleave .> mapBoth (sortBy cmpint) .> go
      where
        go (xs, ys)
            = output [part1, part2]
              where
                ysCount = b_fromList cmpint ys                                  || count occurences of values in list2
                part1   = zipWith (-) xs ys |> map abs |> sum |> showint
                part2   = map similarityScore xs |> sum |> showint

                similarityScore x = x * b_findWithDefault cmpint 0 x ysCount
