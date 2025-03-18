%export day12

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import <set>
%import <state>

cave       == string
connection == (cave, cave)
path       == [cave]
caveMap    == m_map cave path
pathState  == (cave, bool, s_set cave)  || state used as key for pathCount cache

smallCave :: cave -> bool
smallCave = isLower . hd

mkCaveMap :: [connection] -> caveMap
mkCaveMap
    = foldl addCxn m_empty
      where
        addCxn m (a, b) = (addPath a b . addPath b a) m
        addPath a b     = m_insertWith cmpstring (++) a [b]

pathCount :: bool -> caveMap -> cave -> int
pathCount part2 m cave
    = fst (go False s_empty cave m_empty)
      where
        go visit2 seen cave cache
            = (0, cache),         if blocked & (~part2 \/ cave ==$ "start" \/ visit2)
            = (1, cache),         if cave ==$ "end"
            = (cached, cache),    if isJust mcached
            = (computed, cache2), otherwise
              where
                st           = (cave, visit2, seen)
                mcached      = m_lookup cmppathState st cache
                cached       = fromJust mcached
                blocked      = s_member cmpstring cave seen
                (ns, cache1) = st_mapM (go visit2' seen') (m_findWithDefault cmpstring [] cave m) cache
                computed     = sum ns
                cache2       = m_insert cmppathState st computed cache1
                visit2'      = visit2 \/ blocked
                seen'        = s_insert cmpstring cave seen, if smallCave cave
                             = seen,                          otherwise

parseConnection :: string -> connection
parseConnection
    = mkCxn . span letter
      where
        mkCxn (s1, s2) = (s1, tl s2)

day12 :: io ()
day12
    = readFile "../inputs/day12.txt" >>=. go
      where
        go input
            = io_mapM_ putStrLn [part1, part2]
              where
                caves = mkCaveMap . map parseConnection . lines $ input
                part1 = (++) "part 1: " . showint . pathCount False caves $ "start"
                part2 = (++) "part 2: " . showint . pathCount True  caves $ "start"
