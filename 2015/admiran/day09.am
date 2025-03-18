|| day09.m
||
|| This is a travelling salesman problem; exact solutions can be done by brute force, checking every permutation of every city for min/max length


%export day09

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <map>
%import <mirandaExtensions>


city == string
path == [city]

distanceMap == m_map city (m_map city int)

lookupDistance :: distanceMap -> city -> city -> int
lookupDistance dm c1 c2
    = m_findWithDefault cmpstring 0 c2 . m_findWithDefault cmpstring m_empty c1 $ dm

distanceForPath :: distanceMap -> path -> int
distanceForPath dm cs
    = fst . foldl addDist (0, hd cs) $ tl cs
      where
        addDist (d, c1) c2 = (d + lookupDistance dm c1 c2, c2)

readDistances :: string -> io distanceMap
readDistances fn
    = foldl addEntry m_empty <$>. map words <$>. lines <$>. readFile fn
      where
        addEntry m [c1, _, c2, _, ds]
            = insert c2 c1 . insert c1 c2 $ m
              where
                d = intval ds
                insert a b m
                  = m_insertWith cmpstring (m_union cmpstring) a (m_singleton b d) m

        addEntry m _ = error "readDistances: parse error"

day09 :: io ()
day09
    = readDistances "../inputs/day09.input" >>=. go
      where
        go dm
            = io_mapM_ putStrLn [part1, part2]
              where
                cities = m_keys dm
                dists  = map (distanceForPath dm) . permutations $ cities
                part1  = (++) "part 1: " . showint . min cmpint $ dists
                part2  = (++) "part 2: " . showint . max cmpint $ dists
