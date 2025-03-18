|| day19.m -- Linen Layout


%export day19

%import "adventLib"
%import <map>
%import <maybe>
%import <memo>
%import <state>


|| find the number of ways we can make a pattern from the given towels
waysToMakePattern :: [string] -> string -> memoSt string int
waysToMakePattern ts
    = memo ways
      where
        ways [] = st_pure 1
        ways p
            = sum <$> st_mapM (waysToMakePattern ts) prefixes
              where
                prefixes    = map tryPrefix ts |> catMaybes
                tryPrefix t = stripPrefix cmpchar t p

parseInput :: string -> ([string], [string])
parseInput
    = lines .> splitWhen null .> parse
      where
        parse [[ts], ps] = words ts |> map (filter letter) |> ($pair ps)
        parse _ = error "parse error"

day19 :: io ()
day19
    = readFile "../inputs/day19.txt" >>= parseInput .> go
      where
        go (ts, ps)
            = output [part1, part2]
              where
                nways = st_evalState (st_mapM (waysToMakePattern ts) ps) (cmpstring, m_empty)
                part1 = count (~= 0) nways |> showint
                part2 = sum          nways |> showint
