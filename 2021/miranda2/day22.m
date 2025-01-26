%export day22

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <maybe>
%import <mirandaExtensions>


|| vector definitions
v2 *      == (*, *)
v3 *      == (*, *, *)


|| range operations
v2_range == v2 int

r_size, r_lo, r_hi :: v2_range -> int
r_size (l, h) = max2 cmpint (h - l + 1) 0
r_lo (l, h)   = l
r_hi (l, h)   = h

r_intersect :: v2_range -> v2_range -> v2_range
r_intersect (l1, h1) (l2, h2) = (max2 cmpint l1 l2, min2 cmpint h1 h2)


|| cuboid operations
cuboid == v3 v2_range

c_size :: cuboid -> int
c_size (rx, ry, rz) = product (map r_size [rx, ry, rz])

c_intersect :: cuboid -> cuboid -> cuboid
c_intersect (r1x, r1y, r1z) (r2x, r2y, r2z) = (r_intersect r1x r2x, r_intersect r1y r2y, r_intersect r1z r2z)

c_difference :: cuboid -> cuboid -> [cuboid]
c_difference c1 c2
    = [c1],                                             if c_size ci == 0
    = [],                                               if _eq cmpcuboid ci c1
    = filter ((> 0) . c_size) [s1, s2, s3, s4, s5, s6], otherwise
      where
        ci              = c_intersect c1 c2
        (r1x, r1y, r1z) = c1
        (x1l, x1h)      = r1x
        (y1l, y1h)      = r1y
        (z1l, z1h)      = r1z

        (rix, riy, riz) = ci
        (xil, xih)      = rix
        (yil, yih)      = riy
        (zil, zih)      = riz

        s1         = ((x1l, xil - 1), r1y,            r1z)
        s2         = ((xih + 1, x1h), r1y,            r1z)
        s3         = (rix,            (y1l, yil - 1), r1z)
        s4         = (rix,            (yih + 1, y1h), r1z)
        s5         = (rix,            riy,            (z1l, zil - 1))
        s6         = (rix,            riy,            (zih + 1, z1h))


|| canvas operations
operation ::= Off | On
command   ==  (operation, cuboid)

canvas == [cuboid]

cn_empty :: canvas
cn_empty = []

cn_size :: canvas -> int
cn_size = sum . map c_size

cn_draw :: canvas -> command -> canvas
cn_draw cs (Off, c) =     concatMap ($c_difference c) cs
cn_draw cs (On, c)  = c : concatMap ($c_difference c) cs


readInput :: string -> io [command]
readInput fn
    = map (readCmd . words) <$>. lines <$>. readFile fn
      where
        perror s = error ("parse error: " ++ s)

        readCmd ("on" : [xs])  = (On, readCuboid xs)
        readCmd ("off" : [xs]) = (Off, readCuboid xs)
        readCmd xs             = perror "expected a command"

        readCuboid = mkCuboid . map readAxis . split ','

        mkCuboid [a, b, c] = (a, b, c)
        mkCuboid xs        = perror "expected 3 ranges for cuboid"

        readAxis = mkRange . map intval . splitWhen isDelim

        isDelim c = 'x' <=. c <=. 'z' \/ c ==. '=' \/ c ==. '.'

        mkRange [lo, hi] = (lo, hi)
        mkRange xs       = perror "bad range"

day22 :: io ()
day22
    = readInput "../inputs/day22.txt" >>=. go
      where
        go cmds
            = io_mapM_ putStrLn [part1, part2]
              where
                initr    = (-50, 50)
                initb    = (initr, initr, initr)
                initCmds = filter ((> 0) . c_size . c_intersect initb . snd) cmds
                part1    = (++) "part 1: " . showint . cn_size . foldl cn_draw cn_empty $ initCmds
                part2    = (++) "part 2: " . showint . cn_size . foldl cn_draw cn_empty $ cmds
