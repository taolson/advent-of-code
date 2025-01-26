|| day20 -- added strictness to prevent space leaks

%export day20

%import <avl>
%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <map>
%import <maybe>
%import <mirandaExtensions>


idx   == int
value == int
elt   == (idx, value)
ring  == (int, m_map idx elt)   || ring size, ring map

|| strict(er) version of m_fmapWithKey
m_fmapWithKey' :: (* -> ** -> ***) -> m_map * ** -> m_map * ***
m_fmapWithKey' f
    = go
      where
        go AVLLeaf                = AVLLeaf
        go (AVLNode (k, v) l r h)
            = fkv $seq l' $seq r' $seq AVLNode (k, fkv) l' r' h
              where
                fkv = f k v
                l'  = go l
                r'  = go r



makeRing :: [value] -> ring
makeRing xs
    = (#xs, (m_fromList cmpidx . map mkEntry . zip2 [0 ..]) xs)
      where
        mkEntry (i, v) = (i, (i, v))

mix :: ring -> ring
mix (sz, rm)
    = rm' $seq (sz, rm')
      where
        rm' = foldl step rm [0 .. sz - 1]
        step rm oi
            = m_fmapWithKey' adj rm
              where
                (ci, cv) = fromJust (m_lookup cmpidx oi rm)
                ni       = (ci + cv) $mod (sz - 1)
                adj o (i, v)
                    = (seq ni ni, cv),  if o == oi
                    = (seq ip1 ip1, v), if i <  ci & i >= ni
                    = (seq im1 im1, v), if i >= ci & i <= ni
                    = (i, v),           otherwise
                      where
                        ip1 = i + 1
                        im1 = i - 1
                
coordinates :: ring -> int
coordinates (sz, rm)
    = elt1k + elt2k + elt3k
      where
        elts  = map snd . sortOn cmpint fst . m_elems $ rm
        idx0  = fromJust . elemIndex cmpint 0 $ elts
        elt1k = elts ! ((idx0 + 1000) $mod sz)
        elt2k = elts ! ((idx0 + 2000) $mod sz)
        elt3k = elts ! ((idx0 + 3000) $mod sz)

day20 :: io ()
day20
    = readFile "../inputs/day20.txt" >>=. (go . map intval . lines)
      where
        go input
            = io_mapM_ putStrLn [part1, part2]
              where
                key   = 811589153
                part1 = (++) "part 1: " . showint . coordinates . mix . makeRing $ input
                part2 = (++) "part 2: " . showint . coordinates . (! 10) . iterate mix . makeRing . map (* key) $ input
