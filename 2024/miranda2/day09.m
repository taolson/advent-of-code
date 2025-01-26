|| day09.m -- Disk Fragmenter
||
|| version which uses a vector of heaps for the free list

%export day09

%import "adventLib"
%import <maybe>         (>>=?)/mb_bind
%import <heap>
%import <vector>
 
dirent ::= File int int | Free int
pos == int
spn == (pos, dirent)

isFree :: dirent -> bool
isFree (File _ _) = False
isFree (Free _)   = True

expandDiskMap :: string -> [spn]
expandDiskMap
    = map digitVal .> chunk 2 .> enumerate .> mapAccumL mkEntry 0 .> snd .> concat
      where
        mkEntry p (i, [nb, nf])
            = (p2, [sb, sf])
              where
                p1 = p  + nb
                p2 = p1 + nf
                sb = (p, File i nb)
                sf = (p1, Free nf)
        mkEntry _ _ = undef     || added to remove compiler warning

|| compact the file blocks into the free spans, moving each block in the file to the earliest free span
compactBlocks :: [spn] -> [spn] -> [spn]
compactBlocks
    = go
      where
        go ((pf, Free nf) : fs) ((pe, File i nb) : es)
            = (pe, File i nb) : es, if pf >= pe         || no more free spans left of file spans
            = case cmpint nf nb of
                EQ -> (pf, File i nb) : go fs  es       || file filled free slot
                LT -> (pf, File i nf) : go fs  es'      || split file
                GT -> (pf, File i nb) : go fs' es       || split free slot
              , otherwise
              where
                es' = (pe + nf, File i (nb - nf)) : es
                fs' = (pf + nb, Free (nf - nb))   : fs

        go _ _ = []

|| build a vector of 10 heaps, one for each possible span length
makeFreeHeaps :: [spn] -> vector (heap spn)
makeFreeHeaps
    = foldl ins v
      where
        v = v_rep 10 h_empty

        ins v s
            = v // [(i, h')]
              where
                (_, Free i) = s
                h = v !! i
                h' = h_insert cmpspn s h

|| compact the files using the freeList, moving each file to the earliest Free list entry that can
|| hold it, or leave it in place
compactFiles :: [spn] -> [spn] -> [spn]
compactFiles fs
    = go vhs
      where
        vhs = makeFreeHeaps fs

        || find the left-most free heap of size >= n
        findMinFreeHeap v n
            = map ((v !!) .> h_viewMin cmpspn) [n .. 9] |> catMaybes |> minBy cmpspn fst

        go vhs ((pe, File i nb) : es)
            = findMinFreeHeap vhs nb |> check
              where
                check ((pf, Free nf), h')
                    = (pe, File i nb) : go vhs es, if pf >= pe   || no free spans to the right; leave in place
                    = case cmpint nf nb of
                        EQ -> (pf, File i nb) : go vhs1 es       || file filled free slot
                        LT -> (pe, File i nb) : go vhs  es       || shouldn't happen due to break condition
                        GT -> (pf, File i nb) : go vhs2 es       || split free slot and add remaining to appropriate freeHeap
                      , otherwise
                        where
                          vhs1 = vhs // [(nf, h')]
                          vhs2 = vhs1 // [(nf', h_insert cmpspn (pf', Free nf') (vhs1 !! nf'))]
                                 where
                                   nf' = nf - nb
                                   pf' = pf + nb
                check _ = undef || added to remove compiler warning
        go _ _ = []

checksum :: [spn] -> int
checksum
    = map cs .> sum
      where
        cs (p, File i n) = sum [i * p' | p' <- [p .. p + n - 1]]
        cs _             = 0

day09 :: io ()
day09
    = readFile "../inputs/day09.txt" >>= expandDiskMap .> partition (snd .> isFree) .> mapSnd reverse .> go
      where
        go (frees, files)
            = output [part1, part2]
              where
                part1 = compactBlocks frees files |> checksum |> showint
                part2 = compactFiles  frees files |> checksum |> showint
