|| day22.m


%export day22

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <dequeue>
%import <lens>
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import <parser> (<*)/p_left (*>)/p_right
%import <set>


loc == (int, int)
dir ::= N | E | S | W

step :: loc -> dir -> loc
step (x, y) d
    = case d of
        N -> (x, y + 1)
        E -> (x + 1, y)
        S -> (x, y - 1)
        W -> (x - 1, y)

node == (loc, int, int)

n_loc      = lensTup3_0
n_capacity = lensTup3_1
n_used     = lensTup3_2

gridSt == (loc, loc)    || empty loc, data loc

nodeMap == m_map loc node

makeNodeMap :: [(node, node)] -> nodeMap
makeNodeMap
    = foldl ins m_empty . append . unzip2
      where
        append (a, b) = a ++ b
        ins m n       = m_insert cmploc (view n_loc n) n m

viablePairs :: [node] -> [(node, node)]
viablePairs ns
    = filter viable $ pairs
      where
        pairs = [(n1, n2) | n1, n2 <- ns]

        viable ((l1, c1, u1), (l2, c2, u2))
            = _ne cmploc l1 l2 & u1 > 0 & c2 - u2 > u1

movesFrom :: nodeMap -> gridSt -> [gridSt]
movesFrom m (el, dl)
    = map newState emptyLocs
      where
        emptyLocs    = filter inMap . map (step el) $ [N, E, S, W]
        inMap loc    = m_member cmploc loc m
        newState el' = (el', if' (_eq cmploc el' dl) el dl)

searchPath :: nodeMap -> gridSt -> (gridSt -> bool) -> maybe [gridSt]
searchPath nm start finished
    = go (dq_singleton (start, [])) s_empty
      where
        go q visited
            = Nothing,        if dq_null q
            = Just path',     if finished st
            = go q1 visited,  if s_member cmpgridSt st visited
            = go q2 visited', otherwise
              where
                ((st, path), q1) = fromJust $ dq_viewL q
                path'            = st : path
                visited'         = s_insert cmpgridSt st visited
                expanded         = [(move, path') | move <- movesFrom nm st]
                q2               = foldr dq_addR q1 expanded

p_restOfLine :: parser string
p_restOfLine = p_manyUntil p_any (p_char '\n')

p_header :: parser [string]
p_header = p_count 2 $ p_restOfLine

p_node :: parser node
p_node = p_liftA4 mkNode
                  (p_string "/dev/grid/node-x" *> p_int)
                  (p_string "-y" *> p_int)
                  (p_spaces *> p_int <* p_char 'T')
                  (p_spaces *> p_int <* p_restOfLine)
         where
           mkNode x y c u = ((x,y), c, u)

readNodes :: string -> io [node]
readNodes fn
    = go <$>. parse (p_header *> p_some p_node) <$>. readFile fn
      where
        go (mns, ps) = fromMaybe (error (p_error ps)) mns

day22 :: io ()
day22
    = readNodes "../inputs/day22.input" >>=. go
      where
        go nl
            = io_mapM_ putStrLn [part1, part2]
              where
                pairs  = viablePairs nl
                nm     = makeNodeMap pairs
                en     = minBy cmpint (view n_used) nl
                dn     = maxBy cmpint (fst . view n_loc) . filter ((== 0) . snd . view n_loc) $ nl
                initSt = (view n_loc en, view n_loc dn)
                done   = (_eq cmploc (0, 0)) . snd
                part1  = (++) "part 1: " . showint . length $ pairs
                part2  = (++) "part 2: " . showint . length . fromJust $ searchPath nm initSt done
