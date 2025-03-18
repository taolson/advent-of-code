|| day12m.m -- Hot Springs -- memoized version


%export day12

%import <io> (>>=.)/io_bind
%import <map>
%import <maybe>
%import <memo>
%import <mirandaExtensions>
%import <state> (<$>)/st_fmap


spring          == char
conditionRecord == ([spring], [int])    || springs, size of contiguous damaged groups

findCombos :: conditionRecord -> memoSt conditionRecord int
findCombos = memo findCombos'

findCombos' :: conditionRecord -> memoSt conditionRecord int
findCombos' (s, [])  = st_pure 0, if elem cmpchar '#' s
                     = st_pure 1, otherwise
findCombos' ([], _)  = st_pure 0

findCombos' ((s0 : ss), qa)
    = doOpr,              if s0 ==. '.'
    = doBroken,           if s0 ==. '#'
    = doOpr <+> doBroken, otherwise
      where
        q0 : qs  = qa
        doOpr    = findCombos (ss, qa)
        doBroken = brokenCase ss q0 qs
        (<+>)    = st_liftA2 (+)

        brokenCase srest q qrest
            = st_pure 0, if q - 1 > #srest      || more broken to match, but not enough springs left: no solution
            = check,     if q - 1 == #srest     || same number of broken as springs left; check
            = tally,     otherwise              || more springs than broken left, tally up all possible solutions
              where
                check = st_pure 0, if elem cmpchar '.' srest \/ not (null qrest)        || all remaining must be '#' or '?'
                      = st_pure 1, otherwise                                            || match

                || tally is written as a sum of a list, but the list will only contain 0 or 1 elements;
                || this list comprehension is used to simplify filtering out the bad cases
                tally = sum <$> st_mapM findCombos xs
                        where
                          xs = [(rest, qrest) |
                                (header, dot : rest) <- [splitAt (q - 1) srest];        || header is the match for (q - 1) broken springs
                                dot ~=. '#';                                            || dot is the next elem; must be '.' or '?' (implicit '.')
                                ~elem cmpchar '.' header]                               || header must only be '#' or '?' (implicit '#')

processLine1 :: string -> int
processLine1 str
    = st_evalState (findCombos (fstr, seqnums)) $ (cmp, m_empty)
      where
        cmp (cs1, ns1) (cs2, ns2)
            = cmpint (#cs1) (#cs2) $thenCmp cmpint (#ns1) (#ns2)
 
        [fstr, seqnumsStr] = split ' ' str
        seqnums            = map intval . split ',' $ seqnumsStr

processLine2 :: string -> int
processLine2 str
    = st_evalState (findCombos (fullFstr, fullSeqNums)) $ (cmp, m_empty)
      where
        cmp (cs1, ns1) (cs2, ns2)
            = cmpint (#cs1) (#cs2) $thenCmp cmpint (#ns1) (#ns2)

        [fstr, seqnumsStr] = split ' ' str
        seqnums            = map intval . split ',' $ seqnumsStr
        fullFstr           = intercalate "?" $ rep 5 fstr
        fullSeqNums        = concat $ rep 5 seqnums

day12 :: io ()
day12
    = readFile "../inputs/day12.txt" >>=. go
      where
        go input
            = io_mapM_ putStrLn [part1, part2]
              where
                process f = showint . sum . map f . lines $ input
                part1     = "part 1: " ++ process processLine1
                part2     = "part 2: " ++ process processLine2
