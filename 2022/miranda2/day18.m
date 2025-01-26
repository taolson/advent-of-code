%export day18

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <lens>
%import <maybe>
%import <mirandaExtensions>
%import <set>
%import <v3>

cube        == v3 int
cubeSet     == s_set cube
boundingBox == (cube, cube)
cubeState   == (cubeSet, boundingBox, int)

adjacent :: cube -> [cube]
adjacent c = [(v3_add c . v3_mul dv . v3_pure) di | di <- [-1, 1]; dv <- [V3 1 0 0, V3 0 1 0, V3 0 0 1]]

addCube :: cubeState -> cube -> cubeState
addCube (cs, bbox, sides) c
    = (cs', bbox', sides')
      where
        cs'      = s_insert cmpcube c cs        
        bbox'    = (v3_min cmpint (fst bbox) c, v3_max cmpint (snd bbox) c)
        touching = (length . filter id . map (converse (s_member cmpcube) cs)) (adjacent c)
        sides'   = sides + 6 - 2 * touching

findExternal :: cubeSet -> boundingBox -> int
findExternal cs bb
    = go 0 cs [fst bb]
      where
        go ext seen [] = ext
        go ext seen (x : xs)
            = go ext seen xs,             if s_member cmpcube x seen
            = go ext' seen' (xs ++ exp'), otherwise
              where
                seen'         = s_insert cmpcube x seen
                exp           = filter (inBoundingBox bb) (adjacent x)
                (touch, exp') = partition (converse (s_member cmpcube) cs) exp
                ext'          = ext + #touch

                inBoundingBox (cmin, cmax) c
                    = v3_cmp (>=) c cmin & v3_cmp (<=) c cmax
                      where
                        v3_cmp f a b = v3_foldr (&) True (v3_liftA2 f a b)

readCubes :: string -> io [cube]
readCubes fn
    = map readCube <$>. lines <$>. readFile fn
      where
        readCube         = mkCube . map intval . split ','
        mkCube [x, y, z] = V3 x y z
        mkCube xs        = error "readCubes: bad parse"
      
day18 :: io ()
day18
    = readCubes "../inputs/day18.txt" >>=. go
      where
        go cubes
            = io_mapM_ putStrLn [part1, part2]
              where
                (cs, bbox, sides) = foldl addCube (s_empty, (c0, c0), 0) cubes where c0 = hd cubes
                bbox'             = extend bbox
                part1             = (++) "part 1: " . showint $ sides
                part2             = (++) "part 2: " . showint . findExternal cs $ bbox'

        extend (cMin, cMax)
            = (v3_sub cMin (v3_pure 1), v3_add cMax (v3_pure 1))
