%export day02

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <mirandaExtensions>


shape == int            || Rock == 0, Scissors == 1, Paper == 2
round == (shape, shape) || opponent, me

play :: round -> int
play (op, me)
    = me + 1 + score
      where
        delta = me - op
        score = 3, if delta == 0
              = 6, if delta == 1 \/ delta == -2
              = 0, otherwise

convertRound :: round -> round
convertRound (op, outcome)
    = (op, me)
      where
        me = (op + outcome + 2) $mod 3

readGuide :: string -> io [round]
readGuide fn
    = map readRound <$>. lines <$>. readFile fn
      where
        readRound (a : _ : b : _) = (readShape a, readShape b)
        readRound xs              = undef       || prevent compiler from complaining about missing pattern
        readShape c
            = code c - code 'A', if c <. 'X'
            = code c - code 'X', otherwise

day02 :: io ()
day02
    = readGuide "../inputs/day02.txt" >>=. go
      where
        go guide
            = io_mapM_ putStrLn [part1, part2]
              where
                part1 = (++) "part 1: " . showint . sum . map play $ guide
                part2 = (++) "part 2: " . showint . sum . map play . map convertRound $ guide
