%export day20

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <astar>
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import <set>


point       == (int, int)
maze        == s_set point

portalTile  == (point, string)
portalMap   == m_map point point
portalNames == m_map string point

findEdges :: [portalTile] -> (int, int, int, int)
findEdges (((x, y), c) : tiles)
    = foldl minmax (x, x, y, y) tiles
      where
        minmax (xMin, xMax, yMin, yMax) ((x, y), c) = (min2 cmpint xMin x, max2 cmpint xMax x, min2 cmpint yMin y, max2 cmpint yMax y)

findEdges _ = error "findEdges: no match"

findPortalEdge :: [portalTile] -> (point -> int) -> ((int -> int) -> point -> point) -> int -> int -> [portalTile]
findPortalEdge ports getAxis adjAxis val adj
    = ports'
      where
        p1                        = filter ((== val)       . getAxis . fst) ports
        p2                        = filter ((== val + adj) . getAxis . fst) ports
        portLocs                  = map (adjAxis (+ adj) . fst) p2
        ports'                    = map buildPortal (zip3 portLocs (map snd p1) (map snd p2))
        buildPortal (loc, s1, s2) = (loc, s1 ++ s2), if adj > 0
                                  = (loc, s2 ++ s1), otherwise
        
findPortals :: [portalTile] -> ([portalTile], [portalTile])
findPortals tiles
    = (el ++ er ++ et ++ eb, il ++ ir ++ it ++ ib)
      where
        exEdges          = findEdges tiles
        (exMin, exMax, eyMin, eyMax) = exEdges
        (ep, ip)         = partition (edgePort exEdges) tiles
        el               = findPortalEdge ep fst adjFst exMin 1
        er               = findPortalEdge ep fst adjFst exMax (-1)
        et               = findPortalEdge ep snd adjSnd eyMin 1
        eb               = findPortalEdge ep snd adjSnd eyMax (-1)
        inEdges          = findEdges ip
        (ixMin, ixMax, iyMin, iyMax) = inEdges
        il               = findPortalEdge ip fst adjFst (ixMin + 1) (-1)
        ir               = findPortalEdge ip fst adjFst (ixMax - 1) 1
        it               = findPortalEdge ip snd adjSnd (iyMin + 1) (-1)
        ib               = findPortalEdge ip snd adjSnd (iyMax - 1) 1
        adjFst f (a, b) = (f a, b)
        adjSnd f (a, b) = (a, f b)
        edgePort (xMin, xMax, yMin, yMax) ((x, y), s)
            = True, if xMin <= x <= xMin + 1
            = True, if xMax >= x >= xMax - 1
            = True, if yMin <= y <= yMin + 1
            = True, if yMax >= y >= yMax - 1
            = False, otherwise

makePortalMaps :: [portalTile] -> [portalTile] -> (portalMap, portalMap, portalNames)
makePortalMaps extLst intLst
    = (extMap, intMap, extMapR)
      where
        extMapR          = m_fromList cmpstring (map swapTuple extLst)
        intMapR          = m_fromList cmpstring (map swapTuple intLst)
        extMap           = (m_fromList cmppoint . map (mapSnd intMapR)) extLst
        intMap           = (m_fromList cmppoint . map (mapSnd extMapR)) intLst
        swapTuple (a, b) = (b, a)
        mapSnd m (p, s)  = (p, m_findWithDefault cmpstring (0, 0) s m)

graph == m_map point (m_map point int)

pathToEachPortalFrom :: maze -> portalMap -> portalMap -> point -> m_map point int
pathToEachPortalFrom maze extMap intMap start
    = (m_fromList cmppoint . catMaybes . map backtrace) portalLocs
      where
        portalLocs         = m_keys extMap ++ m_keys intMap
        pathMap            = aStarReachable cmppoint start expFn () costFn estFn
        expFn ((x, y), ()) = (filter inMaze [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)], ())
        costFn _ _         = 1
        estFn _            = 0
        backtrace p        = Nothing,         if _eq cmppoint p start
                           = bt 0 p p,        otherwise
        bt n p q           = Just (p, n),     if _eq cmppoint q start
                           = Nothing,         if isNothing found
                           = bt (n + 1) p q', otherwise
                             where
                               found   = m_lookup cmppoint q pathMap
                               Just q' = found

       inMaze p = s_member cmppoint p maze

makeGraph :: maze -> portalMap -> portalMap -> graph
makeGraph maze extMap intMap
    = foldl addPaths m_empty portalLocs
      where
        addPaths m p = m_insert cmppoint p (pathToEachPortalFrom maze extMap intMap p) m
        portalLocs   = m_keys extMap ++ m_keys intMap
        
graphPathDist :: graph -> [(int, point)] -> int
graphPathDist graph path
    = dist
      where
        (_, dist) = foldl addStep (snd (hd path), 0) (map snd (tl path))
        addStep (p1, t) p2 = (p2, t + m_findWithDefault cmppoint 1 p2 (m_findWithDefault cmppoint m_empty p1 graph))

movesFrom :: graph -> portalMap -> portalMap -> point -> point -> bool -> (int, point) -> [(int, point)]
movesFrom graph extMap intMap entrance exit recursive (level, loc)
    = moves
      where
        moves        = onLevel ++ portalJump
        onLevel      = (map (addLevel level) . m_keys . m_findWithDefault cmppoint m_empty loc) graph
        addLevel l x = (l, x)
        portalJump   = [(level, extPort)],     if isJust extJump & ~recursive
                     = [(level, intPort)],     if isJust intJump & ~recursive
                     = [(level - 1, extPort)], if isJust extJump & recursive & notBlocked
                     = [(level + 1, intPort)], if isJust intJump & recursive
                     = [],                     otherwise
        extJump      = m_lookup cmppoint loc extMap
        Just extPort = extJump
        intJump      = m_lookup cmppoint loc intMap
        Just intPort = intJump
        notBlocked   = _eq cmppoint loc entrance \/ _eq cmppoint loc exit, if level == 0
                     = _ne cmppoint loc entrance & _ne cmppoint loc exit,  otherwise

move == (int, point)

solveMaze :: graph -> portalMap -> portalMap -> point -> point -> bool -> [(int, point)]
solveMaze graph extMap intMap entrance exit recursive
    = path
      where
        hugeint                = 999999
        (path, ())             = aStarSolve cmpmove (0, entrance) goalFn expandFn () costFn estFn
        goalFn (level, p)      = level == 0 & _eq cmppoint p exit
        expandFn (loc, ())     = (movesFrom graph extMap intMap entrance exit recursive loc, ())
        costFn (l1, a) (l2, b) = 1, if extJmp \/ intJmp
                               = cost, if isNothing graphFind
                               = hugeint, otherwise
                                 where
                                   graphFind = m_lookup cmppoint b (m_findWithDefault cmppoint m_empty a graph)
                                   Just cost = graphFind
                                   extJmp    = _eq cmppoint b (m_findWithDefault cmppoint (0, 0) a extMap)
                                   intJmp    = _eq cmppoint b (m_findWithDefault cmppoint (0, 0) a intMap)
        estFn _                = 0

readMaze :: string -> io (maze, [portalTile])
readMaze fn
    = go <$>. lines <$>. readFile fn
      where
        go rows
            = foldl addLoc (s_empty, []) [(x, y, c) | (y, row) <- enumerate rows; (x, c) <- enumerate row]

        addLoc (m, p) (x, y, c)
            = (s_insert cmppoint (x, y) m, p), if c ==. '.'
            = (m, ((x, y), [c]) : p),          if letter c
            = (m, p),                          otherwise

day20 :: io ()
day20
    = readMaze "../inputs/day20.input" >>=. go
      where
        go (maze, portals)
            = io_mapM_ putStrLn [part1, part2]
              where
                (extLst, intLst)              = findPortals portals
                (extMap, intMap, portalNames) = makePortalMaps extLst intLst
                Just entrance                 = m_lookup cmpstring "AA" portalNames 
                Just exit                     = m_lookup cmpstring "ZZ" portalNames 
                graph                         = makeGraph maze extMap intMap
                process                       = showint . graphPathDist graph . solveMaze graph extMap intMap entrance exit
                part1                         = "part 1: " ++ process False
                part2                         = "part 2: " ++ process True
