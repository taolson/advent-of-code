|| day22.m


%export day22

%import <io> (>>.)/io_right
%import <astar>
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import <set>
%import <state> (>>=)/st_bind (<$>)/st_fmap (<<)/st_left (>>)/st_right


region == (int, int)

giCache == m_map region int

origin = (0, 0)
target = (13, 734)
depth  = 7305

geologicIndex :: region -> state giCache int
geologicIndex region
    = st_get >>= search
      where
        (x, y) = region

        search cache
            = fromMaybef (computeIndex >>= updateCache) st_pure (m_lookup cmpregion region cache)

        computeIndex
            = st_pure 0,           if _eq cmpregion region origin
            = st_pure 0,           if _eq cmpregion region target
            = st_pure (x * 16807), if y == 0
            = st_pure (y * 48271), if x == 0
            = st_liftA2 (*) (erosionLevel (x, y - 1)) (erosionLevel (x - 1, y)), otherwise

        updateCache gi
            = st_pure gi << st_modify (m_insert cmpregion region gi)

erosionLevel :: region -> state giCache int
erosionLevel region
    = computeLevel <$> geologicIndex region
      where
        computeLevel index = (index + depth) $mod 20183

regionType :: region -> state giCache int
regionType region
    = ($mod 3) <$> erosionLevel region

riskLevel :: state giCache int
riskLevel
    = sum <$> st_mapM regionType [(x, y) |  x <- [0 .. fst target]; y <- [0 .. snd target]]

tool ::= None | Torch | Gear

move == (region, tool)

toolsForRegion :: region -> state giCache [tool]
toolsForRegion region
    = choose <$> regionType region
      where
        choose 0 = [Gear, Torch]
        choose 1 = [Gear, None]
        choose 2 = [Torch, None]
        choose _ = error "toolsForRegion: bad regionType"

adjacentMoves :: move -> state giCache [move]
adjacentMoves (region, tool)
    = toolsForRegion region >>= adj
      where
        adj tools
            = st_filterM canUseTool $ moves ++ switches
              where
                (x, y)   = region
                moves    = [(region', tool) | region' <- [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]; fst region' >= 0; snd region' >= 0]
                switches = [(region, tool') | tool' <- tools]

                canUseTool (r, t)
                    = toolsForRegion r >>= check
                      where
                        check tools' = st_pure $ member cmptool tools' t

timeToMove :: move -> move -> int
timeToMove (r, t) (r', t')
    = max2 cmpint (dist r r')  1 + if' (_eq cmptool t t') 0 6
      where
        dist (x, y) (x', y') = abs (x - x') + abs (y - y')

findPath :: move -> move -> state giCache [move]
findPath start goal cache
    = aStarSolve cmpmove start goalFn expandFn cache costFn estimateFn
      where
        goalFn m   = _eq cmpmove m goal
        expandFn (m, c) = (adjacentMoves m c)
        costFn     = timeToMove
        estimateFn = timeToMove goal

day22 :: io ()
day22
    = putStrLn part1 >>. putStrLn part2
      where
        (risk, giCache) = st_runState riskLevel m_empty
        bestPath        = st_evalState (findPath (origin, Torch) (target, Torch)) giCache
        moves           = zip2 bestPath $ tl bestPath
        time            = sum . map (uncurry timeToMove) $ moves
        part1           = "part 1: " ++ showint risk
        part2           = "part 2: " ++ showint time
