|| day11.m -- Plutonian Pebbles


%export day11

%import "adventLib"
%import <bag>


stone   == int
stoneSt == b_bag stone

transform :: stone -> [stone]
transform n
    = [1],                 if n == 0
    = split2 s |> toStone, if even (#s)
    = [n * 2024],          otherwise
      where
        s = showint n

        toStone (a, b) = map intval [a, b]

blink :: stoneSt -> stoneSt
blink st = b_fromCountList cmpint [(s', n) | (s, n) <- b_toList st; s' <- transform s]

day11 :: io ()
day11
    = readFile "../inputs/day11.txt" >>= words .> map intval .> b_fromList cmpint .> go
      where
        go stoneSt
            = output [part1, part2]
              where
                process n = iterate blink stoneSt ! n |> b_elems |> sum |> showint
                part1     = process 25
                part2     = process 75
