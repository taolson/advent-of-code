%export day14

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <map>
%import <maybe>
%import <mirandaExtensions>


elt          == char
eltPair      == (elt, elt)
reactionMap  == m_map eltPair elt
insertionMap == m_map eltPair int
eltBag       == m_map elt int

step :: reactionMap -> (insertionMap, eltBag) -> (insertionMap, eltBag)
step rm (im, hist)
    = foldl react (m_empty, hist) (m_toList im)
      where
        react (im, hist) (p, n)
            = (im1, hist),  if isNothing mr
            = (im2, hist'), otherwise
              where
                mr             = m_lookup cmpeltPair p rm
                cr             = fromJust mr
                (c1, c2)       = p
                im1            = addPairs n p im
                im2            = (addPairs n (c1, cr) . addPairs n (cr, c2)) im
                hist'          = m_insertWith cmpchar (+) cr n hist
                addPairs n p m = m_insertWith cmpeltPair (+) p n m

tallyElts :: eltBag -> int
tallyElts hist
    = mst - lst
      where
        counts          = m_elems hist
        (lst, mst)      = foldl minmax (c1, c1) counts
        c1              = hd counts
        minmax (a, b) c = (min2 cmpint a c, max2 cmpint b c)

readInput :: string -> io (insertionMap, reactionMap, eltBag)
readInput fn
    = go <$>. p_inp <$>. lines <$>. readFile fn
      where
        go (ts, rs)
            = (im, rm, hist)
              where
                rules = map (mkRule . words) rs
                rm    = m_fromList cmpeltPair rules
                im    = foldl addPair m_empty (zip2 ts (tl ts))
                hist  = foldl addElt m_empty ts

        p_inp (ts : x : rs) = (ts, rs)
        p_inp xs            = undef                     || dummy pattern to remove non-exhaustive pattern warning

        mkRule [[c1, c2], x, [c3]] = ((c1, c2), c3)
        mkRule xs                  = undef              || dummy pattern to remove non-exhaustive pattern warning

        addPair m p = m_insertWith cmpeltPair (+) p 1 m
        addElt m c  = m_insertWith cmpchar (+) c 1 m

day14 :: io ()
day14
    = readInput "../inputs/day14.txt" >>=. go
      where
        go (im, rm, hist)
            = io_mapM_ putStrLn [part1, part2]
              where
                steps = iterate (step rm) (im, hist)
                part1 = (++) "part 1: " . showint . tallyElts . snd . (! 10) $ steps
                part2 = (++) "part 2: " . showint . tallyElts . snd . (! 40) $ steps
