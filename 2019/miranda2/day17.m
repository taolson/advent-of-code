%export day17

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <bfs>
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import "intcode"


point == (int, int)

segType ::= Vert | Horiz | Bot

segment ::=
     Segment
     segType    || seg_type
     num        || seg_loc
     num        || seg_start
     num        || seg_end

getSegType :: segment -> segType
getSegType (Segment st sl ss se) = st

rowState == ([segment], m_map num num, string)
colState == ([segment], m_map num num, num, char, num)
rowIdx   == (string, num)

findSegments :: [string] -> [segment]
findSegments [] = []
findSegments rows
    = segments
      where
        (segments, _, _) = foldl processRow startRowState indexedRows
        startRowState    = ([], m_empty, padRow)
        indexedRows      = zip2 (rows ++ [padRow]) [0 ..]
        padRow           = rep (# (hd rows)) '.'

processRow :: rowState -> rowIdx -> rowState
processRow (segs, vStarts, prev) (row, y)
    = (segs', vStarts', row)
      where
        (segs', vStarts', _, _, _) = foldl (processCol y) startColState cols
        startColState                    = (segs, vStarts, 0, '.', 0)
        cols                             = zip2 (padCol prev) (padCol row)
        padCol r                         = r ++ "."
        
processCol :: num -> colState -> (char, char) -> colState
processCol y (segs, vStarts, hStart, cl, x) (pc, cc)
    = (segs', vStarts', hStart', cc, x + 1)
      where
        startHSeg = cc ~=. cl & cl ==. '.'
        endHSeg   = cc ~=. cl & cc ==. '.' & x - hStart > 1
        startVSeg = cc ~=. pc & pc ==. '.'
        endVSeg   = cc ~=. pc & cc ==. '.' & y - xStart > 1
        bot       = cc ~=. '.' & cc ~=. '#'
        xStart    = m_findWithDefault cmpint 0 x vStarts
        newHSeg   = Segment Horiz y hStart (x - 1)
        newVSeg   = Segment Vert  x xStart (y - 1)
        newBSeg   = Segment Bot   x y      y
        segsH     = newHSeg : segs,       if endHSeg
                  = segs,                 otherwise
        segsV     = newVSeg : segsH,      if endVSeg
                  = segsH,                otherwise
        segs'     = newBSeg : segsV,      if bot
                  = segsV,                otherwise
        vStarts'  = m_insert cmpint x y vStarts, if startVSeg
                  = vStarts,              otherwise
        hStart'   = x,                    if startHSeg
                  = hStart,               otherwise

intersect :: segment -> segment -> maybe (num, num)
intersect (Segment type1 loc1 start1 end1) (Segment type2 loc2 start2 end2)
    = Nothing,           if _eq cmpsegType type1 type2
    = Nothing,           if loc1 <= start2 \/ loc1 >= end2
    = Nothing,           if loc2 <= start1 \/ loc2 >= end1
    = Just (loc1, loc2), otherwise

abuts :: segment -> segment -> bool
abuts (Segment type1 loc1 start1 end1) (Segment type2 loc2 start2 end2)
    = False, if _eq cmpsegType type1 type2
    = False, if loc1 ~= start2 & loc1 ~= end2
    = False, if loc2 ~= start1 & loc2 ~= end1
    = True,  otherwise

connections :: segment -> [segment] -> [segment]
connections seg = filter (abuts seg)

findIntersections :: [segment] -> [point]
findIntersections segments
    = catMaybes maybeLocs
      where
        (hSegs, vSegs) = (partition ((_eq cmpsegType Horiz) . getSegType) . filter ((_ne cmpsegType Bot) . getSegType)) segments
        maybeLocs      = [intersect h v | h <- hSegs; v <- vSegs]

command == (char, num)

findPathFrom :: segment -> [segment] -> [command]
findPathFrom seg1 segments
    = [],                                                      if null connects
    = (turn, dist) : findPathFrom seg2 (delete cmpsegment seg1 segments), otherwise
      where
        connects                       = connections seg1 segments
        seg2 : _                      = connects
        Segment type1 loc1 start1 end1 = seg1
        Segment _ loc2 start2 end2 = seg2
        dist                           = end2 - start2
        turn                           = 'R', if _eq cmpsegType type1 Horiz & loc1 == start2 & loc2 == end1
                                       = 'R', if _eq cmpsegType type1 Horiz & loc1 == end2 & loc2 == start1
                                       = 'R', if _eq cmpsegType type1 Vert & loc1 == start2 & loc2 == start1
                                       = 'R', if _eq cmpsegType type1 Vert & loc1 == end2 & loc2 == end1
                                       = 'L', otherwise
        
patternsUpTo :: num -> [command] -> m_map [command] num
patternsUpTo n commands
    = foldl (addPatterns commands) m_empty [1 .. n]
      where
        addPatterns commands pats n
            = pats,                                                                 if # commands < n
            = addPatterns (tl commands) (m_insertWith cmp (+) (take n commands) 1 pats) n, otherwise
              where
                cmp = cmplist cmpcommand


uniqueCombinations :: num -> [*] -> [[*]]
uniqueCombinations n elts
    = [[]],                                                                 if n == 0 \/ n > # elts
    = [elts],                                                               if n == # elts
    = map (e :) (uniqueCombinations (n - 1) es) ++ uniqueCombinations n es, otherwise
      where
        (e : es) = elts

scmd == (string, [command])

sequenceForPath :: [command] -> [[command]] -> [scmd]
sequenceForPath path pats
    = path'
      where
        cmp                   = cmplist cmpcommand
        (path', ())           = bfsSolve cmpscmd ([], path) goalFn expandFn ()
        goalFn (s, p)         = null p
        expandFn ((s, p), ()) = ((map (buildPat s p) . filter (matchPat p)) annotatedPats, ())
        buildPat s p (c, pat) = (s ++ [c], drop (# pat) p)
        matchPat p (c, pat)   = _eq cmp (take (# pat) p) pat
        annotatedPats         = zip2 "ABC" pats

findSequenceAndPatternSet :: [command] -> [[command]] -> (string, [string])
findSequenceAndPatternSet path patterns
    = (seqString seq, map patString pats)
      where
        patSets             = uniqueCombinations 3 patterns
        (pats, seqPath)     = (hd . filter (not . null . snd) . map addSeq) patSets
        addSeq pats         = (pats, sequenceForPath path pats)
        seq                 = (fst . last) seqPath
        patString pat       = (intercalate "," . map addSteps) pat ++ "\n"
        addSteps (t, steps) = t : ',' : shownum steps
        seqString seq       = (intersperse ',' seq) ++ "\n"

readView :: jitState -> [string]
readView t
    = view,         if isHalt (jitGetRunState t')
    = view ++ rest, otherwise
      where
        (out, t') = (jitGetAllOutput . jitContinue) t
        view      = (filter ((> 0) . length) . lines . map decode) out
        rest      = readView t'

runRobot :: jitState -> string -> [string] -> num
runRobot t seq pats
    = dust, if isHalt (jitGetRunState t2)
    = -999, otherwise
      where
        t1        = jitContinue (jitSetMem 0 2 t)
        (out, t2) = (jitGetAllOutput . foldl sendCommand t1) (seq : pats ++ ["n\n"])
        dust      = last out
        sendCommand t cmd
                  = jitContinue (jitPutAllInput t' (map code cmd))
                    where
                      (_, t') = jitGetAllOutput t

day17 :: io ()
day17
    = readProgram "../inputs/day17.input" >>=. (go . jitReset)
      where
        go t
            = io_mapM_ putStrLn [part1, part2]
              where
                segments        = (findSegments . readView) t
                intersections   = findIntersections segments
                bot             = (hd . filter ((_eq cmpsegType Bot) . getSegType)) segments
                path            = findPathFrom bot segments
                patterns        = m_keys (patternsUpTo 5 path)
                (seq, pats)     = findSequenceAndPatternSet path patterns
                dust            = runRobot t seq pats
                mulTuple (x, y) = x * y
                part1           = (++) "part 1: " . showint . sum . map mulTuple $ intersections
                part2           = (++) "part 2: " . showint $ dust
