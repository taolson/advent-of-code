%export day22

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <maybe>
%import <mirandaExtensions>
%import <parser> (<$>)/p_fmap (<<)/p_left (>>)/p_right (<|>)/p_alt


shuffle ::= Rev | Cut num | Deal num

linear ::= Linear
           num          || modulo
           num          || multiplicand
           num          || addend

modLinear :: linear -> num
modLinear (Linear m a b) = m

unitLinear :: num -> linear
unitLinear m = Linear m 1 0

appendLinear :: linear -> linear -> linear
appendLinear (Linear m a b) (Linear _ c d)
    = seq a' (seq b' (Linear m a' b'))
      where
        a' = (c * a) $mod m
        b' = (b * c + d) $mod m

powLinear :: linear -> num -> linear
powLinear a n
    = powLinear (recipLinear a) (- n),                        if n <  0
    = unitLinear (modLinear a),                               if n == 0
    = a,                                                      if n == 1
    = seq a2 (powLinear a2 (n $div 2)),                        if even n
    = seq a2 (appendLinear a (powLinear a2 ((n - 1) $div 2))), otherwise
      where
        a2 = appendLinear a a

recipLinear :: linear -> linear
recipLinear a = powLinear a (modLinear a - 2)

evalLinear :: linear -> num -> num
evalLinear (Linear m a b) x = (a * x + b) $mod m

solveLinear :: linear -> num -> num
solveLinear a y = evalLinear (recipLinear a) y

shuffleToLinear :: num -> shuffle -> linear
shuffleToLinear m Rev      = Linear m (-1) (-1)
shuffleToLinear m (Cut n)  = Linear m   1  (-n)
shuffleToLinear m (Deal n) = Linear m   n    0

|| day22 parsing

p_shuffle :: parser shuffle
p_shuffle
    = (p_pure Rev << p_string "deal into new stack" << p_spaces) <|>
      (Cut  <$> (p_string "cut " >> p_int << p_spaces))          <|>
      (Deal <$> (p_string "deal with increment " >> p_int << p_spaces))

readShuffles :: string -> io [shuffle]
readShuffles fn
    = go <$>. parse (p_many p_shuffle << p_end) <$>. readFile fn
      where
        go (ms, ps) = fromMaybe (error (p_error ps)) ms

day22 :: io ()
day22
    = readShuffles "../inputs/day22.input" >>=. go
      where
        go shuffles
            = io_mapM_ putStrLn [part1, part2]
              where
                shuffle1 = makeCombinedShuffle 10007
                shuffle2 = powLinear (makeCombinedShuffle 119315717514047) 101741582076661
                part1    = (++) "part 1: " . showint . evalLinear shuffle1 $ 2019
                part2    = (++) "part 2: (error due to int64? should be 43781998578719) " . showint . solveLinear shuffle2 $ 2020

                makeCombinedShuffle m
                    = foldl appendLinear (unitLinear m) (map (shuffleToLinear m) shuffles)
