|| day21.m -- uses vector table lookup of bitpatterns


%export day21

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <mirandaExtensions>
%import <v2>
%import <vector>


pos == v2 int

(+@), (-@) :: pos -> pos -> pos
a +@ b = v2_add a b
a -@ b = v2_sub a b


|| a pattern is a list of ON positions and a size
pattern ::= Pattern [pos] int

patSize :: pattern -> int
patSize (Pattern _ sz) = sz

readPattern :: string -> pattern
readPattern s
    = Pattern ps (#rows)
      where
        rows = split '/' s
        ps = [V2 r c | (r, row) <- enumerate rows; (c, ch) <- enumerate row; ch ==. '#']

|| split a pattern into a list of NxN patterns
splitN :: int -> pattern -> [pattern]
splitN n (Pattern ps sz)
    = map split blocks
      where
        blocks = [V2 r c | r, c <- [0, n .. sz - 1]]

        split block
            = Pattern ps' n
              where
                ps' = filter inBlock0 . map (-@ block) $ ps

                inBlock0 (V2 r c) = 0 <= r < n & 0 <= c < n

|| join a list of NxN patterns into one (N*2)x(N*2) pattern
joinN :: int -> [pattern] -> pattern
joinN n pts
    = Pattern ps' (n * 2)
      where
        ps' = concat $ zipWith adj pts [V2 r c | r, c <- [0, n]]

        adj (Pattern ps _) block = map (+@ block) ps

|| transform a pattern with all possible flips and rotations
allXforms :: pattern -> [pattern]
allXforms (Pattern ps sz)
    = map doXform $ rots ++ map (flip .) rots
      where
        flip (V2 r c) = V2 (sz - r - 1) c
        rot  (V2 r c) = V2 c (sz - r - 1)

        rots = [id, rot, rot . rot, rot . rot . rot]

        doXform xf
            = Pattern (map xf ps) sz

bitvec == int

patternToBits :: pattern -> bitvec
patternToBits (Pattern ps sz)
    = foldl addLoc 0 ps
      where
        addLoc n (V2 r c) = n .|. (1 .<<. (r * sz + c))
         
bitsToPattern :: int -> bitvec -> pattern
bitsToPattern sz bp
    = Pattern ps sz
      where
        ps = filter (hasLoc bp) [(V2 r c) | r, c <- [0 .. sz - 1]]

        hasLoc bp (V2 r c) = (bp .&. (1 .<<. (r * sz + c))) ~= 0

|| count the number of '1' bits in a bitvec
bitCount :: bitvec -> int
bitCount bv
    = sum . map extractBit $ [0 .. 15]   || highest bit set in a bitvec is 15 (a 4x4 pattern)
      where
        extractBit i = (bv .>>. i) .&. 1


|| a rule is a mapping from a matching pattern to a replacement pattern
rule == (pattern, pattern)
readRule :: string -> rule
readRule s
    = (readPattern a, readPattern b)
      where
        [a, b] = splitOneOf cmpchar " =>" s

|| create a list of rule mappings from all the possible transforms of a rule pattern to its result
|| the first parameter is a function on the result pattern to split it
ruleToBits :: (pattern -> [pattern]) -> rule -> [(bitvec, [bitvec])]
ruleToBits f (pa, pb)
    = map ($pair bbs) bas
      where
        bas = map patternToBits $ allXforms pa
        bbs = map patternToBits $ f pb

|| a mapping from a bitvec matching a rule pattern to the bitvecs of its expansion (either 1 or 4)
|| uses an O(1) vector lookup
rulevec == vector [bitvec]

|| the step state machine cycles between 3 states:
|| S2: expand a single 2x2 bitvec to a single 3x3 bitvec
|| S3: expand a single 3x3 bitvec to a 4x4 bitvec, then split into four 2x2 bitvecs
|| S4: expand four 2x2 bitvecs to 4 3x3 bitvecs, then join them to a 6x6 pattern, then split that to 9 2x2 patterns
||     then expand each of those to a 2x2
stepState ::= S2x2 | S3x3 | S4x4

onAfterSteps :: int -> (rulevec, rulevec) -> pattern -> int
onAfterSteps steps (rs2, rs3) p
    = step S3x3 steps [patternToBits p]
      where
        singleton x = [x]

        step S2x2 0 [bv] = bitCount $ bv
        step S2x2 n [bv] = step S3x3 (n - 1) (rs2 !! bv)

        step S3x3 0 [bv] = bitCount $ bv
        step S3x3 n [bv] = step S4x4 (n - 1) (rs3 !! bv)

        step S4x4 0 bvs  = sum . map bitCount $ bvs
        step S4x4 n bvs
            = sum . map (step S2x2 (n - 1) . singleton) $ bvs'
              where
                bvs' = map patternToBits . splitN 2 . joinN 3 . map (bitsToPattern 3 . hd . (rs2 !!)) $ bvs

        step _ _ _ = error "step: bad step parameters"

readRules :: string -> io (rulevec, rulevec)
readRules fn
    = go <$>. partition ((== 2) . patSize . fst) <$>. map readRule <$>. lines <$>. readFile fn
      where
        go (p2s, p3s)
            = (rs2, rs3)
              where
                rs2 = v_rep (2 ^ (2 * 2)) [] // concatMap (ruleToBits (splitN 3)) p2s    || lookup and expansion of a 2x2 to a single 3x3
                rs3 = v_rep (2 ^ (3 * 3)) [] // concatMap (ruleToBits (splitN 2)) p3s    || lookup and expansion of a 3x3 to four 2x2s

day21 :: io ()
day21
    = readRules "../inputs/day21.input" >>=. go
      where
        go rules
            = io_mapM_ putStrLn [part1, part2]
              where
                pat   = readPattern $ ".#./..#/###"
                part1 = (++) "part 1: " . showint . onAfterSteps 5  rules $ pat
                part2 = (++) "part 2: " . showint . onAfterSteps 18 rules $ pat
