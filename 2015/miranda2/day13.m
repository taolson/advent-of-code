|| day13.m


%export day13

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import <vector>


name  == string
hmap  == m_map name (m_map name int)
smap  == vector (vector int)
table == [int]



|| convert an hmap, with O(logN) lookup on names to a vector of vectors, with O(1) lookup
toSmap :: hmap -> smap
toSmap hm
    = v_fromList . map (mkEntry . m_toList) . m_elems $ hm
      where
        sz   = m_size hm
        nmap = m_fromList cmpname $ zip2 (m_keys hm) [0 ..]

        remap (n, x) = (m_findWithDefault cmpname 0 n nmap, x)
        mkEntry xs   = v_rep sz 0 // map remap xs


tableDelta :: smap -> table -> int
tableDelta sm ixs
    = foldl go 0 $ zip2 ixs (tl ixs ++ [hd ixs])
      where
        hdelt ix1 ix2 = sm !! ix1 !! ix2
        go t (n1, n2) = t + hdelt n1 n2 + hdelt n2 n1

readInput :: string -> io hmap
readInput fn
    = foldl ins m_empty <$>. map words <$>. lines <$>. readFile fn
      where
        parseError = error "readInput: parse error"

        ins m (n1 : _ : gl : d : ws)
            = m_insertWith cmpname (m_union cmpname) n1 (m_singleton n2 $ delta gl) m
              where
                n2 = init . last $ ws

                delta "gain" =  intval d
                delta "lose" = -intval d
                delta _      = parseError

         ins _ _ = parseError

day13 :: io ()
day13
    = readInput "../inputs/day13.input" >>=. go
      where
        process v = showint . max cmpint . map (tableDelta v) . permutations $ [0 .. v_length v - 1]

        go hm
            = io_mapM_ putStrLn [part1, part2]
              where
                  part1 = (++) "part 1: " . process . toSmap $ hm
                  part2 = (++) "part 2: " . process . toSmap $ m_insert cmpname "self" m_empty hm

