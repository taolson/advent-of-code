|| day25.m -- Code Chronicle


%export day25

%import "adventLib"


locKey ::= Lock [int] | Key [int]

isLock :: locKey -> bool
isLock (Lock _) = True
isLock _        = False

cols :: locKey -> [int]
cols (Lock cols) = cols
cols (Key  cols) = cols

parse :: string -> ([locKey], [locKey])
parse = lines .> splitWhen null .> map mkLocKey .> partition isLock
        where
          mkLocKey xs
             = mkLock xs', if hd (hd xs) ==. '#'
             = mkKey  xs', otherwise
               where
                 xs' = xs |> drop 1 |> take 5
          
          mkLock = transpose .> map (span  (==. '#') .> fst .> length) .> Lock
          mkKey  = transpose .> map (break (==. '#') .> snd .> length) .> Key
 
countFits :: (int -> bool) -> [locKey] -> [locKey] -> int
countFits tst locks keys
    = sum [1 | l <- locks; k <- keys; all tst (zipWith (+) (cols l) (cols k))]
        
day25 :: io ()
day25
    = readFile "../inputs/day25.txt" >>= parse .> go
      where
        go (locks, keys)
            = output [part1, part2]
              where
                part1 = countFits (<= 5) locks keys |> showint
                part2 = countFits (== 5) locks keys |> showint
