%export day14

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <lens>
%import <maybe>
%import <mirandaExtensions>
%import <parser> (<$>)/p_fmap
%import <set>
%import <maybeState> (>>=)/mst_bind (<<)/mst_left (<|>)/mst_alt
%import <v2>


point == v2 int
cave  == (s_set point, int)

|| parsing
p_point :: parser point
p_point = p_liftA2 V2 (p_int $p_left p_char ',') p_int

p_path :: parser [point]
p_path
    = mkPath <$> p_someSepBy (p_string " -> ") p_point
      where
        mkPath ps      = (concatMap mkSeg . zip2 ps) (tl ps)
        mkSeg (p1, p2) = takeWhile (_ne cmppoint p2) (iterate (v2_add delta) p1) ++ [p2] where delta = v2_signum (v2_sub p2 p1)

pourSand :: bool -> point -> cave -> int
pourSand noFloor source (s, my)
    = (subtract (s_size s) . s_size . snd . dfs source) s
      where
        dfs p
            = mst_fail,                                             if noFloor & y > my
            = (mst_get >>= checkBlocked) <|> checkPaths, otherwise
              where
                V2 x y   = p
                y'       = y + 1
                paths    = [V2 x y', V2 (x - 1) y', V2 (x + 1) y']

                checkBlocked s
                    = mst_pure (), if y > my + 1 \/ s_member cmppoint p s
                    = mst_fail,    otherwise

                checkPaths = mst_mapM_ dfs paths << mst_modify (s_insert cmppoint p)

|| read the cave from the input, returning it and the maximum y value processed
readCave :: string -> io cave
readCave fn
    = go <$>. parse (p_manySepBy p_spaces p_path) <$>. readFile fn
      where
        go (pr, ps)       = fromMaybef (error $ p_error ps) mkCave pr
        mkCave            = foldl insCave (s_empty, -1) . concat
        insCave (s, my) p = (s_insert cmppoint p s, max2 cmpint my (view lensV2_1 p))

day14 :: io ()
day14
    = readCave "../inputs/day14.txt" >>=. go
      where
        go cave
            = io_mapM_ putStrLn [part1, part2]
              where
                source = V2 500 0
                part1  = (++) "part 1: " . showint . pourSand True  source $ cave
                part2  = (++) "part 2: " . showint . pourSand False source $ cave
