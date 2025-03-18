|| day24.m -- Crossed Wires


%export day24

%import "adventLib"
%import <map>
%import <maybe>         (<|>?)/mb_alt
%import <set>
%import <state>


op ::= AND | OR | XOR

makeOp :: string -> op
makeOp "AND" = AND
makeOp "OR"  = OR
makeOp "XOR" = XOR
makeOp _     = error "makeOp: unknown op"

signal  == string
gate    == (op, signal, signal, signal) || op, s1, s2, d
circuit == m_map signal gate            || map from signal to the gate that generates it
refs    == m_map signal [gate]          || map from signal to the gates that use it
input   == (signal, int)

(!!) :: m_map signal * -> signal -> *
c !! s = fromJust $ m_lookup cmpsignal s c

parseNets :: string -> ([input], (circuit, refs))
parseNets
    = lines .> break null .> (map mkInput *** (drop 1 .> foldl insCircuit (m_empty, m_empty)))
      where
        err = error "parse error"

        mkInput
            = splitOneOf cmpchar ": " .> go
              where
                go [s, n] = (s, intval n)
                go _      = err

        insCircuit (circuit, refs)
            = words .> go
              where
                go [s1, op, s2, _, d]
                    = (circuit', refs')
                      where
                        g        = (makeOp op, s1, s2, d)
                        circuit' = m_insert cmpsignal d g circuit
                        refs'    = foldl ins refs [s | s <- [s1, s2]]
                        ins m s  = m_insertWith cmpsignal (++) s [g] m

                go _ = err

evalCache == m_map signal int

|| evaluate the signal in the circuit, memoizing its value in the cache
eval :: circuit -> signal -> state evalCache int
eval circuit s
    = st_get >>= check
      where
        check cache
            = case m_lookup cmpsignal s cache of
                  Nothing -> compute (circuit !! s) >>= update
                  Just v  -> st_pure v

        update v = st_pure v << st_modify (m_insert cmpsignal s v)

        compute (op, s1, s2, _) = st_liftA2 (fnFor op) (eval circuit s1) (eval circuit s2)

        fnFor AND = (.&.)
        fnFor OR  = (.|.)
        fnFor XOR = (.^.)

|| ints are lsb -> msb
binval :: [int] -> int
binval xs  = foldr go 0 xs where go b n = n .<<. 1 .|. b


|| find gates who's output wires have been swapped.  This means that the gate type and the inputs must be correct
|| but its output may go to the wrong place.
|| infer the gates of the circuit based upon matching the gates of a ripple-carry adder, where the
|| full adder gates are:
|| P    = X ^ Y
|| G    = X & Y
|| Z    = P ^ C         == P on first stage
|| T    = P & C         == nothing on first stage
|| C'   = G | T         == G on first stage

dest :: gate -> signal
dest (_, _, _, d) = d

sources :: gate -> [signal]
sources (_, s1, s2, _) = [s1, s2]

isXY :: signal -> bool
isXY (c : _) = c ==. 'x' \/ c ==. 'y'
isXY _       = False

|| a gate's role in the adder can be determined by looking at its op and
|| its first source
isP, isG, isZ, isT, isC :: gate -> bool
isP (XOR, s, _, _) = isXY s
isP _              = False

isG (AND, s, _, _) = isXY s
isG _              = False

isZ (XOR, s, _, _) = ~isXY s
isZ _              = False

isT (AND, s, _, _) = ~isXY s
isT _              = False

isC (OR, _, _, _)  = True
isC _              = False

|| report a gate as being swapped by adding it to the swapped set in the state
addSwapped :: gate -> state (s_set signal) ()
addSwapped g = st_modify (dest g |> s_insert cmpstring)

|| walk the circuit up from bit 0, verifying each full adder's connectivity
|| and reporting the gates that have swapped outputs
verify :: circuit -> refs -> [signal] -> [signal] -> state (s_set signal) (maybe signal)
verify circuit refs xs zs
    = zip2 xs zs |> enumerate |> st_foldM go Nothing
      where
        go msc (i, (sx, sz))
            = checkP >>= checkZ >>= checkC >>= checkT >>= checkG >>= genC
              where

                || check P gate connectivity through to Z
                checkP
                    = st_pure rslt,                 if vp
                    = addSwapped p >> st_pure rslt, otherwise
                      where
                        pgs  = refs !! sx               || get the P and G gates from the references to sx
                        p    = fromJust $ find isP pgs  || guaranteed to exist, as it can't be swapped
                        g    = fromJust $ find isG pgs
                        dp   = dest p
                        mz   = find isZ $ refs !! dp
                        zg   = fromJust mz
                        vp   = i == 0 & dp ==$ sz \/ isJust mz & member cmpsignal (sources zg) dp

                        z    = p,             if i == 0 || stage0 has no carry in, so z == p
                             = zg,            if vp     || use the discovered z, if valid p
                             = circuit !! sz, otherwise || use the z connected to sz, otherwise

                        rslt = (Just p,  g, z), if vp
                             = (Nothing, g, z), otherwise
                      
                || verify the discovered Z gate is the sz signal
                checkZ rslt
                    = st_pure rslt,   if vz
                    = addSwapped z >> st_pure rslt, otherwise
                      where
                        (_, _, z) = rslt
                        vz        = dest z ==$ sz

                || check C gate connectivity from Z
                checkC (mp, g, z)
                    = st_pure rslt,                 if i == 0 \/ vc
                    = addSwapped c >> st_pure rslt, otherwise
                      where
                        sc = fromJust msc
                        c  = circuit !! sc
                        vc = isJust msc & member cmpsignal (sources z) sc

                        rslt = (mp, g, Just c),  if vc
                             = (mp, g, Nothing), otherwise

                || check T gate connectivity from P or C, depending upon which we know is not swapped
                checkT (mp, g, mc)
                    = st_pure (g, mp),        if i == 0              || T == P in stage 0
                    = checkFrom (mp <|>? mc), otherwise
                      where
                        checkFrom Nothing = st_pure (g, Nothing)        || both p and c swapped -- can't find T!
                        checkFrom (Just x)
                            = st_pure rslt,                 if vt
                            = addSwapped t >> st_pure rslt, otherwise
                              where
                                mt   = find isT $ refs !! dest x
                                t    = fromJust mt
                                mtrs = m_lookup cmpsignal (dest t) refs || dest t could be an output not in the refs!
                                trs  = fromJust  mtrs
                                vt   = isNothing mt \/ isJust mtrs & isJust (find isC $ trs)
                                rslt = (g, mt)

                || check G gate connectivity to a C gate
                checkG (g, mt)
                    = st_pure (Just g, mt),                  if vg
                    = addSwapped g >> st_pure (Nothing, mt), otherwise
                      where
                        mcrs = m_lookup cmpsignal (dest g) refs
                        crs  = fromJust  mcrs
                        vg   = i == 0 \/ isJust mcrs & isJust (find isC crs)

                || generate the C' signal from g or t, depending upon which we know is not swapped
                genC (mg, mt)
                    = genFrom (mg <|>? mt)
                      where
                        genFrom Nothing  = st_pure Nothing
                        genFrom (Just x)
                            = st_pure (Just $ dest x), if i == 0
                            = st_pure (Just $ dest c), if isJust mc
                            = st_pure Nothing,         otherwise
                              where
                                mc = find isC $ refs !! dest x
                                c  = fromJust mc

day24 :: io ()
day24
    = readFile "../inputs/day24.txt" >>= parseNets .> go
      where
        go (inputs, (circuit, refs))
            = output [part1, part2]
              where
                zs    = m_keys circuit |> filter (hd .> (==. 'z'))
                xs    = m_keys refs    |> filter (hd .> (==. 'x'))
                cache = m_fromList cmpsignal inputs
                part1 = st_evalState (st_mapM (eval circuit) zs) cache |> binval |> showint
                part2 = st_execState (verify circuit refs xs zs) s_empty |> s_toList |> intercalate ","
