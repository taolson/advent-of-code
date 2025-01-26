|| day07.m -- Bridge Repair


%export day07

%import "adventLib"

|| concatenate digits of two ints
|| faster to compute numerically by scaling first number by power of 10 of the second,
|| rather than converting both to strings, concatenating, then converting back to an int
(&&) :: int -> int -> int
a && b = a * pow10 b + b where pow10 n = iterate (* 10) 10 |> dropWhile (<= n) |> hd

checkEqn :: [int -> int -> int] -> [int] -> bool
checkEqn ops (r : ns)
    = ~null (go ns)
      where
        go (a : b : ns) = [n | op <- ops; a $op b <= r; n <- go (a $op b : ns); n == r]
        go ns           = ns

checkEqn _ _ = error "bad eqn"

day07 :: io ()
day07
    = readFile "../inputs/day07.txt" >>= lines .> map (words .> map intval) .> go
      where
        go eqns
            = output [part1, part2]
              where
                part1 = process [(+), (*)]
                part2 = process [(+), (*), (&&)]

                process ops = filter (checkEqn ops) eqns |> map hd |> sum |> showint
