|| day05.m -- Print Queue


%export day05

%import "adventLib"
%import <either>
%import <map>


|| dependencyGraph operations
|| note: I initially attempted to use the dependencyGraph module from the miranda2 compiler, but
|| it determined total ordering over the graph and detected many cycles during findSCCs.  Turns out
|| for this problem we only need the forward dependencies and determine partial ordering on each of
|| the updates

dependencyGraph == m_map int [int]

makeDependencyGraph :: [(int, int)] -> dependencyGraph
makeDependencyGraph
    = foldl addDep m_empty
      where
        addDep g (a, b) = m_insertWith cmpint (++) a [b] g

|| modified form of Ksaraju's Strongly-Connected Components algorithm; only does DFS on the fwd dependencies,
|| filtering the dependencies to only include those in the original list
dependencyOrder :: dependencyGraph -> [int] -> [int]
dependencyOrder g xs
    = foldl dfs ([], []) xs |> fst         || perform a depth-first search on each of the elements of xs, inserting in the proper order
      where
        dfs (ns, vs) n
            = (ns, vs),       if member cmpint vs n     || element n already visited
            = (n : ns', vs'), otherwise                 || insert it after all of its dependents
              where
                (ns', vs') = foldl dfs (ns, n : vs) refs
                refs       = m_findWithDefault cmpint [] n g |> filter (member cmpint xs)       || only follow refs in the original xs list

inCorrectOrder :: dependencyGraph -> [int] -> either [int] [int]
inCorrectOrder g xs
    = Right ordered, if _eq (cmplist cmpint) ordered xs
    = Left  ordered, otherwise
      where
        ordered = dependencyOrder g xs

makeDependencyInfo:: string -> (dependencyGraph, [[int]])
makeDependencyInfo
    = lines .> splitWhen null .> parse
      where
        err = error "parse error"

        mkDep [a, b]  = (a, b)
        mkDep _       = err

        parseGraph     = map (split '|' .> map intval .> mkDep) .> makeDependencyGraph
        parseUpdates   = map (split ',' .> map intval)

        parse [gs, us] = (parseGraph gs, parseUpdates us)
        parse _        = err

day05 :: io ()
day05
    = readFile "../inputs/day05.txt" >>= makeDependencyInfo .> go
      where
        go (g, uds)
            = output [part1, part2]
              where
                (bad, good) = map (inCorrectOrder g) uds |> partitionEithers
                process     = map middleElt .> sum .> showint
                middleElt   = split2 .> snd .> hd
                part1       = process good
                part2       = process bad
