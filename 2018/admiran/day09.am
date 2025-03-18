|| day09.m


%export day09

%import <io> (>>.)/io_right
%import <mirandaExtensions>
%import "ring"


game ::= Game (ring int) (ring int) int int

makeGame :: int -> int -> game
makeGame players lastMarble
    = Game (Ring [] 0 []) (Ring [] 0 (rep (players - 1) 0)) 1 lastMarble

play :: game -> int
play (Game circle scores marbleCount lastMarble)
    = r_foldl (max2 cmpint) 0 scores, if marbleCount >= lastMarble
    = play game',                    otherwise
      where
        marbleCount' = marbleCount + 1

        circle1
          = (iterate r_left circle) ! 7,          if marbleCount $mod 23 == 0
          = r_add marbleCount . r_right $ circle, otherwise

        circle2
          = r_delete circle1, if marbleCount $mod 23 == 0
          = circle1,          otherwise

        scores'
          = r_modify inc . r_right $ scores, if marbleCount $mod 23 == 0
          = r_right scores,                otherwise
            where inc = (+ marbleCount + r_current circle1)

          game' = circle1 $seq circle2 $seq scores' $seq marbleCount' $seq Game circle2 scores' (marbleCount + 1) lastMarble

day09 :: io ()
day09
    = putStrLn part1 >>. putStrLn part2
      where
        part1 = (++) "part 1: " . showint . play $ makeGame 411 72059
        part2 = (++) "part 2: " . showint . play $ makeGame 411 (72059 * 100)
