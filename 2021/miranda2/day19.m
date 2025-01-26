%export day19

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import <set>


|| vector operations
v3 == (int, int, int)

v3_add, v3_mul :: v3 -> v3 -> v3
v3_add (x0, y0, z0) (x1, y1, z1) = (x0 + x1, y0 + y1, z0 + z1)
v3_sub (x0, y0, z0) (x1, y1, z1) = (x0 - x1, y0 - y1, z0 - z1)
v3_mul (x0, y0, z0) (x1, y1, z1) = (x0 * x1, y0 * y1, z0 * z1)

v3_sum :: v3 -> int
v3_sum (x, y, z) = x + y + z

v3_dist :: v3 -> v3 -> int
v3_dist (x0, y0, z0) (x1, y1, z1)
    = dx + dy + dz
      where
        dx = abs (x1 - x0)
        dy = abs (y1 - y0)
        dz = abs (z1 - z0)

v3_fromList :: [int] -> v3
v3_fromList [x, y, z] = (x, y, z)
v3_fromList xs        = error ("v3_fromList: bad list " ++ showlist showint xs)

v3_toList :: v3 -> [int]
v3_toList (x, y, z) = [x, y, z]


|| matrix operations
m3 == (v3, v3, v3)

v3_m3_mul :: v3 -> m3 -> v3
v3_m3_mul v (vx, vy, vz)
    = (x', y', z')
      where
        x' = v3_sum (v3_mul v vx)
        y' = v3_sum (v3_mul v vy)
        z' = v3_sum (v3_mul v vz)

m3_m3_mul :: m3 -> m3 -> m3
m3_m3_mul (vx, vy, vz) m
    = (vx', vy', vz')
      where
        vx' = v3_m3_mul vx m
        vy' = v3_m3_mul vy m
        vz' = v3_m3_mul vz m

v3_m3_madd :: (m3, v3) -> v3 -> v3
v3_m3_madd (m, vo) va = v3_add vo (v3_m3_mul va m)

|| list of all possible rotation matrices
|| generators run through all combinations of 3 single turns, but generate a lot of duplicates
|| these are de-duplicated by running through a set, then adding the final rotation which is
|| missing from the 3 single-turn combinations
rotations :: [m3]
rotations
    = s_toList (s_fromList cmpm3 rg4)
      where
        id = ( ( 1,  0,  0)
             , ( 0,  1,  0)
             , ( 0,  0,  1) )

        rx = ( ( 1,  0,  0)
             , ( 0,  0,  1)
             , ( 0, -1,  0) )

        ry = ( ( 0,  0,  1)
             , ( 0,  1,  0)
             , (-1,  0,  0) )

        rz = ( ( 0,  1,  0)
             , (-1,  0,  0)
             , ( 0,  0,  1) )

        rg0 = [id, rx, ry, rz]
        rg1 = [m3_m3_mul a b | a <- [id]; b <- rg0]
        rg2 = [m3_m3_mul a b | a <- rg1; b <- rg0]
        rg3 = [m3_m3_mul a b | a <- rg2; b <- rg0]
        rg4 = [m3_m3_mul a b | a <- rg3; b <- rg0]


beacon == v3

|| a beaconSig is a set of the distances between a beacon and all the others that a
|| particular scanner sees, since that is invariant with respect to its relative coordinates
|| as seen by the scanner
beaconSig == s_set int

|| a beaconSigMap is a mapping from a beacon's relative coordinates to the scanner to it's beaconSig
|| a beaconMap maps relative locations of becons seen from one scanner to another (without rotations or translations)
beaconSigMap == m_map beacon beaconSig
beaconMap    == m_map beacon beacon

mkBeaconSigMap :: (int, [beacon]) -> beaconSigMap
mkBeaconSigMap (x, beacons)
    = foldl mkSig m_empty beacons
      where
        mkSig m s = m_insert cmpbeacon s (s_fromList cmpint (map (v3_dist s) beacons)) m

|| try to match the beacons seen by two scanners by running a correlation on all their
|| respective beaconSigs, and picking the ones that have at least 10 beacons in common
matchBeacons :: beaconSigMap -> beaconSigMap -> beaconMap
matchBeacons sm1 sm2
    = (m_fmap fst . m_filter cmpbeacon ((>= 10) . snd) . m_fmap matchBeacon) sm1
      where
        sm2l                   = s_toList sm2
        matchBeacon sig        = (maxBy cmpint snd . map (corrSig sig)) sm2l
        corrSig sig1 (s, sig2) = (s, s_size (s_intersect cmpint sig1 sig2))


|| a mapping is a rotation matrix and a translation vector
mapping == (m3, v3)

|| find a possible mapping between a list of corresponding beacons
findMapping :: [beacon] -> [beacon] -> (m3, v3)
findMapping b1s b2s
    = tryRot rotations
      where
        bpairs    = zip2 b1s b2s
        tryRot [] = error "findMapping: no rotation maps the beacons"
        tryRot (r : rs)
            = (r, vo),   if all (_eq cmpv3  vo) offsets
            = tryRot rs, otherwise
              where
                offsets   = map (offset r) bpairs
                vo        = hd offsets
        offset r (b1, b2) = v3_sub b1 (v3_m3_mul b2 r)

|| find the full list of beacons by finding remappings of relative scanner positions
remapAll :: [beaconSigMap] -> ([beacon], [v3])
remapAll smaps
    = go (hd smaps) (tl smaps) []                                       || remap all to the first scanner's relative coordinate system
      where
        go refMap []            slocs = (m_keys refMap, slocs)
        go refMap (smap : rest) slocs
            = go refMap' rest (vo : slocs),      if m_size bmap >= 12          || enough info to remap this smap
            = go refMap (rest ++ [smap]) slocs , otherwise                     || not enough yet, try again later
              where
                (bcns, sigs)    = unzip2 (m_toList smap)
                bmap            = matchBeacons refMap smap
                rm              = findMapping (m_keys bmap) (m_elems bmap)
                (_, vo)         = rm
                bcns'           = map (v3_m3_madd rm) bcns
                refMap'         = foldl addBcn refMap (zip2 bcns' sigs)
                addBcn m (k, v) = m_insertWith cmpbeacon (s_union cmpint) k v m

allPairs :: [*] -> [(*, *)]
allPairs []       = []
allPairs (x : xs) = map (pair x) xs ++ allPairs xs

readInput :: string -> io [(int, [v3])]
readInput fn
    = readScanners <$>. words <$>. readFile fn
      where
        perror s = error ("parse error: " ++ s)

        readScanners []                       = []
        readScanners ("---" : "scanner" : xs) = s : readScanners xs' where (s, xs') = readScanner xs
        readScanners xs                       = perror "expected '---'"

        readScanner (ns : "---" : xs) = ((intval ns, map readVec vs), xs') where (vs, xs') = break (==$ "---") xs
        readScanner xs                = perror "expected a scanner intber"

        readVec = v3_fromList . map intval . split ','

day19 :: io ()
day19
    = readInput "../inputs/day19.txt" >>=. go
      where
        go scans
            = io_mapM_ putStrLn [part1, part2]
              where
                bsigs         = map mkBeaconSigMap scans
                (bcns, slocs) = remapAll bsigs
                part1         = (++) "part 1: " . showint . length $ bcns
                part2         = (++) "part 2: " . showint . max cmpint . map (uncurry v3_dist) . allPairs $ slocs
