|| -*- mode: indented-text -*-
||
|| day22.m -- Monkey Market
||
|| initially written with maps and sets to tally the sequence keys and values, but it is much
|| faster (20x!) to convert the sequences into hashed keys, and pre-allocate a local and global
|| mutable vector to hold the tallys


%export day22

%import "adventLib"
%import <vector>
%import <state>

prand :: int -> int
prand n
    = c
      where
        a = ((n .<<. 6)  .^. n) .&. 0xff_ffff
        b = ((a .>>. 5)  .^. a) .&. 0xff_ffff
        c = ((b .<<. 11) .^. b) .&. 0xff_ffff


|| instead of keeping a window of 4 consecutive delta values to use as a key for map lookups, we
|| create a sliding window of the hash of the values
windowDeltasHash :: [int] -> [(int, int)]
windowDeltasHash
    = scanl hash (0, 0) .> drop 4
      where
        || since each value is 0 .. 9, the deltas are -9 .. 9, so if we add 10
        || to each value, the deltas are 1 .. 19; then scale each delta by 20 to maintain
        || a sequence of consecutive deltas, and mod with (20^4) to keep 4 of them
        hash (k, a) b
            = ((k * 20 + b' - a + 10) $mod 160000, b')
              where
                b' = b $mod 10

tallySequences :: mvector int -> mvector bool -> int -> st ()
tallySequences globalTally localTally n
    = v_fill localTally False >> st_mapM_ tally genSequences
      where
         genSequences = iterate prand n |> windowDeltasHash |> take 2001

         || check the localTally for a sequence hash; if it doesn't exist, update the globalTally with it's value and
         || mark in the localTally as seen
         tally (k, v)
             = v_unsafeRead localTally k >>= check
               where
                 check False = v_unsafeWrite localTally k True >> v_unsafeModify globalTally (+ v) k
                 check True  = st_pure ()

day22 :: io ()
day22
    = readFile "../inputs/day22.txt" >>= words .> map intval .> go
      where
        go ns
            = output [part1, part2]
              where
                maxHash     = 20 * 20 * 20 * 20   || four (-9 .. 9) delta values
                globalTally = v_rep maxHash 0     |> v_unsafeThaw
                localTally  = v_rep maxHash False |> v_unsafeThaw
                part1       = ns |> map (iterate prand .> (! 2000)) |> sum |> showint
                part2       = st_evalState doTally () |> v_max cmpint |> showint
                doTally     = st_mapM_ (tallySequences globalTally localTally) ns >> v_unsafeFreeze globalTally
