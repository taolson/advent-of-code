|| day13.m -- Claw Contraptions


%export day13

%import "adventLib"
%import <lens>
%import <maybe>
%import <v2>            (|+|)/v2_add

xy   == v2 int
game == (xy, xy, xy)

parseGames :: string -> [game]
parseGames
    = lines .> splitWhen null .> map parseGame
      where
        parseGame = map parseXY .> mkGame
        parseXY   = break (==. ':') .> snd .>  break (==. ',') .> mapBoth (filter digit .> intval) .> uncurry V2

        mkGame [a, b, x] = (a, b, x)
        mkGame _         = error "parse error"

adjustPrize :: game -> game
adjustPrize g = overTup3_2 (|+| v2_pure 10_000_000_000_000) g

|| solve for positive integer solutions to 2 simultaneous eqns
|| there is at most 1 positive integer soln for each problem in the set
||
|| zx = A ax + B bx   * by
|| zy = A ay + B by   * bx
|| zx * by - zy * bx = A (ax * by) - (ay * bx)
|| A = dz / da
|| zx = A ax + B bx
|| zx - A ax = B bx
|| (zx - A ax) / bx = B
solveGame :: game -> maybe xy
solveGame (V2 ax ay, V2 bx by, V2 zx zy)
    = Nothing,         if ra ~= 0
    = Just $ V2 na nb, otherwise
      where
       dz = zx * by - zy * bx           || cancel out B coefficient in eqn subtraction
       da = ax * by - ay * bx
       na = dz $div da                  || solve for A coefficient
       ra = dz $mod da                  || remainder of A (only valid if 0)
       db = zx - na * ax                || solve for B, given A
       nb = db $div bx

cost :: xy -> int
cost (V2 a b) = a * 3 + b

day13 :: io ()
day13
    = readFile "../inputs/day13.txt" >>= parseGames .> go
      where
        go games
            = output [part1, part2]
              where
                process = map solveGame .> catMaybes .> map cost .> sum .> showint
                part1   = process games
                part2   = process $ map adjustPrize games
