|| day11.m


%export day11

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <heap>
%import <maybe>
%import <mirandaExtensions>
%import <set>


floor      == int
chipgen    == (floor, floor)            || chip floor, gen floor

chipFloor, genFloor :: chipgen -> floor
chipFloor (cf, _) = cf
genFloor  (_, gf) = gf

exposed :: chipgen -> chipgen -> bool
exposed (c1, g1) (c2, g2) = c1 == g2 & c1 ~= g1 \/ c2 == g1 & c2 ~= g2

addLegal :: chipgen -> [[chipgen]] -> [[chipgen]]
addLegal cg cgs
    = case filter (not . any (exposed cg)) cgs of
        cgs' -> map (cg : ) cgs'


buildingSt == (floor, [chipgen])        || elevator floor, all chip-gen pairs

|| note: the chipgen list should be sorted to eliminate duplicate movesTo expansion
makeBuildingSt :: int -> [chipgen] -> buildingSt
makeBuildingSt fl cgs = (fl, sortBy cmpchipgen cgs)

estCost :: buildingSt -> int
estCost (_, cgs)
    = sum . map cgCost $ cgs
      where
        cgCost (cf, gf) = 4 - min2 cmpint cf gf

movesTo :: floor -> buildingSt -> [buildingSt]
movesTo newf (oldf, cgs)
    = [],    if newf < 1 \/ newf > 4
    = moves, otherwise
      where
        moves = case foldl findMove ([], [], []) cgs of
                  (_, singleMoves, doubleMoves) -> map (makeBuildingSt newf) $ doubleMoves ++ singleMoves

        findMove (nm, sm, dm) cg
            = nm1 $seq sm2 $seq dm2 $seq (nm1, sm2, dm2)
              where
                cf  = fst cg
                gf  = snd cg
                nm1 = cg : nm
                sm1 = addLegal cg sm
                dm1 = addLegal cg dm

                sm2 = sm1 ++ addLegal moveChip [nm] ++ addLegal moveGen [nm], if cf == gf & cf == oldf
                    = sm1 ++ addLegal moveChip [nm],                          if cf == oldf
                    = sm1 ++ addLegal moveGen  [nm],                          if gf == oldf
                    = sm1,                                                    otherwise

                dm2 = dm1 ++ addLegal moveChip sm ++ addLegal moveGen sm ++ addLegal moveBoth [nm], if cf == gf & cf == oldf
                    = dm1 ++ addLegal moveChip sm,                                                  if cf == oldf
                    = dm1 ++ addLegal moveGen  sm,                                                  if gf == oldf
                    = dm1,                                                                          otherwise

                moveChip = (newf, gf)
                moveGen  = (cf,   newf)
                moveBoth = (newf, newf)
   
movesFrom :: buildingSt -> [buildingSt]
movesFrom st
    = movesTo (fl + 1) st ++ movesTo (fl - 1) st
      where
        fl = fst st


hSt == (int, int, buildingSt)

getPathLen :: hSt -> int
getPathLen (pl, _, _) = pl

getCost :: hSt -> int
getCost (_, cost, _) = cost

getSt :: hSt -> buildingSt
getSt (_, _, st) = st

searchPath :: buildingSt -> buildingSt -> int
searchPath start finish
    = go (h_singleton (0, estCost start, start)) s_empty
      where
        cmp hst1 hst2 = cmpint (getPathLen hst1) (getPathLen hst2) $thenCmp cmpint (getCost hst1) (getCost hst2)

        go h visited
            = -999,           if h_null h
            = pl,             if _eq cmpbuildingSt st finish
            = go h1 visited,  if s_member cmpbuildingSt st visited
            = go h2 visited', otherwise
              where
                (hSt, h1) = fromJust $ h_viewMin cmp h

                pl       = getPathLen hSt
                st       = getSt hSt
                visited' = s_insert cmpbuildingSt st visited
                expanded = [(pl + 1, estCost move, move) | move <- movesFrom st]
                h2       = foldr (h_insert cmp) h1 expanded

day11 :: io ()
day11
    = io_mapM_ putStrLn [part1, part2]
      where
        start1  = makeBuildingSt 1 [(1,1), (1,1), (3,2), (2,2), (2,2)]
        finish1 = (4, rep 5 (4, 4))
        part1   = (++) "part 1: " . showint $ searchPath start1 finish1
        start2  = makeBuildingSt 1 [(1,1), (1,1), (1,1), (1,1), (3,2), (2,2), (2,2)]
        finish2 = (4, rep 7 (4, 4))
        part2   = (++) "part 2: " . showint $ searchPath start2 finish2
