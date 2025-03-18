|| day24.m


%export day24

%import <io>    (>>=.)/io_bind (<$>.)/io_fmap
%import <maybe> (>>=?)/mb_bind (<$>?)/mb_fmap (<|>?)/mb_alt
%import <mirandaExtensions>


|| find all combinations of the packages that match the given weight, starting with the given group size
combsMatchingWeightFrom :: int -> int -> [int] -> [[int]]
combsMatchingWeightFrom weight start xs
    = concatMap go [start .. #xs]
      where
        go n = filter ((== weight) . sum) . combinations n $ xs

findBestPartition :: int -> [int] -> [[int]]
findBestPartition parts pkgs
    = fromJust $ go 1 1 pkgs
      where
        weight       = sum pkgs $div parts
        sameSize a b = #a == #b

        go pn sz pkgs
            = Just [pkgs],                                 if pn == parts   || final partition matches weight, by definition
            = firstJust . map partitionWithFirst $ cands', otherwise        || find first valid partition from candidates
              where
                firstJust = foldr (<|>?) Nothing

                || find the candidates for this partition (first partition with smallest size)
                cands  = combsMatchingWeightFrom weight sz pkgs
                cands' = sortOn cmpint product . hd . groupBy sameSize $ cands, if pn == 1      || for first partition, search from lowest quantum entanglement
                       = cands,                                                 otherwise       || for remaining partitions, just search in order

                || given a partition, find all subsequent partitions for it or return Nothing
                partitionWithFirst cand
                    = (cand :) <$>? go (pn + 1) (#cand) (filter (not . member cmpint cand) pkgs)

day24 :: io ()
day24
    = readFile "../inputs/day24.input" >>=. (go . map intval . lines)
      where
        go pkgs
            = io_mapM_ putStrLn [part1, part2]
              where
                process n = showint . product . hd . findBestPartition n $ pkgs
                part1 = "part 1: " ++ process 3
                part2 = "part 2: " ++ process 4
