|| day10.m -- Pipe Maze, using vector for the grid, and the shoelace algorithm for the inside area


%export day10

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <lens>
%import <maybe>
%import <mirandaExtensions>
%import <set>
%import <vector>


loc == (int, int)

dir ::= N | S | E | W

oppositeDir :: dir -> dir
oppositeDir N = S
oppositeDir S = N
oppositeDir E = W
oppositeDir W = E

move :: dir -> loc -> loc
move N (x, y) = (x, y - 1)
move S (x, y) = (x, y + 1)
move E (x, y) = (x + 1, y)
move W (x, y) = (x - 1, y)


|| a pipe is defined by the directions you can move from it
pipe == (dir, dir)

toPipe :: char -> pipe
toPipe '|' = (N, S)
toPipe '-' = (E, W)
toPipe 'L' = (N, E)
toPipe 'J' = (N, W)
toPipe '7' = (S, W)
toPipe 'F' = (S, E)
toPipe c   = error ("toPipe: can't handle " ++ showchar c)

|| pick the next direction to move in a pipe, based upon the direction we came in
|| 
nextDirFrom :: dir -> pipe -> maybe dir
nextDirFrom d (a, b)
    = Just b, if _eq cmpdir d' a
    = Just a, if _eq cmpdir d' b
    = Nothing, otherwise
      where
        d' = oppositeDir d


grid == (vector (maybe pipe), int, int) || pipes, width, height

g_pipes  = lensTup3_0
g_width  = lensTup3_1
g_height = lensTup3_2

g_at :: grid -> loc -> maybe pipe
g_at g (x, y) = view g_pipes g !! (y * view g_width g + x)

g_withOnly :: grid -> path -> grid
g_withOnly (v, w, h) ies
    = (v' // (map toIndex ies), w, h)
      where
        v'                  = v_rep (v_length v) Nothing
        toIndex ((x, y), p) = (y * w + x, Just p)

path == [(loc, pipe)]

|| find the loop beginning at the start location, trying all directions until we find the loop
|| return the path with the inferred start location pipe type
findLoop :: grid -> loc -> path
findLoop g start
    = (start, (oppositeDir d2, d1)) : path
      where
        (d1, (d2, path)) = fromJust . foldr try Nothing $ [N, E, S, W]

        try d k
            = (step [] start d $mb_bind (Just . pair d)) $mb_alt k
              where
                step pth loc d
                    = Just (d, pth),             if _eq cmploc loc' start
                    = g_at g loc' $mb_bind next, otherwise
                      where
                        loc'   = move d loc
                        next p = nextDirFrom d p $mb_bind step pth' loc'
                                 where pth' = (loc', p) : pth

|| shoelace algorithm for finding the area of a polygon given its vertices
|| A = 1/2 * abs sum [xi yi+1 - xi+1yi]
shoelaceArea :: path -> int
shoelaceArea loop
    = a2 $div 2
      where
        locs = map fst loop
        locs' = locs ++ [hd locs]       || append the first onto the end
        a2    = abs . foldl kernel 0 $ zip2 locs' (tl locs')

        kernel s ((x1, y1), (x2, y2))
            = s + x1 * y2 - x2 * y1

|| read the grid and start location
readGrid :: string -> io (grid, loc)
readGrid fn
    = (go . lines) <$>. readFile fn
      where
        go rows
            = (g, s)
              where
                height   = #rows
                width    = #hd rows
                (si, ps) = mapAccumL process 0 . enumerate . concat $ rows
                g        = (v_fromList ps, width, height)
                s        = (si $mod width, si $div width)

        process s (i, c)
            = (s', p)
              where
                s' = i,               if c ==. 'S'
                   = s,               otherwise
                p  = Nothing,         if c ==. '.' \/ c ==. 'S'
                   = Just $ toPipe c, otherwise

day10 :: io ()
day10
    = readGrid "../inputs/day10.txt" >>=. go
      where
        go (grid, start)
            = io_mapM_ putStrLn [part1, part2]
              where
                loop  = findLoop grid start
                nloop = #loop        
                part1 = ("part 1: " ++) . showint $ nloop $div 2
                part2 = ("part 2: " ++) . showint $ shoelaceArea loop - nloop $div 2 + 1
