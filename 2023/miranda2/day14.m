|| day14.m -- Parabolic Reflector Dish


%export day14

%import <io> (>>=.)/io_bind (<$>.)/io_fmap (>>.)/io_right
%import <map>
%import <maybe>
%import <mirandaExtensions>


loc      == (int, int)
rock     == (char, loc)

isSquare :: rock -> bool
isSquare ('#', _) = True
isSquare _        = False


tiltNorth :: [rock] -> [rock]
tiltNorth
    = snd . mapAccumL tilt (0, 0) . sortOn cmploc snd   || ascending x then ascending y
      where
        || move the round rocks one at a time, leaving the square rocks where they are
        || since input is sorted, we can just compare the rock against the running move-to position
        tilt (tx, ty) (rt, (rx, ry))
            = (tloc', (rt, rloc'))
              where
                tx'      = max2 cmpint tx rx             || new move-to x

                ty'      = 0,  if tx' ~= tx & rt ~=. '#' || reset move-to y if we have jumped a column, to 0 if not square rock
                         = ry, if rt ==. '#'             || or to rock location, if square rock
                         = ty, otherwise

                tloc'    = (tx', ty' + 1)                || new move-to location
                rloc'    = (rx, ty')                     || new rock location

|| rotate the rocks clockwise
rotCW :: loc -> [rock] -> [rock]
rotCW (mx, my)
    = map rot
      where
        rot (rt, (x, y)) = (rt, (mx - y - 1, x))

|| tilt N, W, S, E by performing 4 iterations of a tiltNorth, rotCW pair
spinCycle :: loc -> [rock] -> [rock]
spinCycle limit rocks = iterate (rotCW limit . tiltNorth) rocks ! 4

load :: loc -> [rock] -> int
load (_, my)
    = foldl add 0
      where
        add s (rt, (x, y))
            = s,          if rt ==. '#'
            = s + my - y, otherwise

findCycle :: loc -> [rock] -> int
findCycle limit rocks
    = go m_empty fps
      where
        loads =  map (load limit) . iterate (spinCycle limit) $ rocks

        || fingerprints of 4 consecutive spinCycle load values, along with their index
        fps   = enumerate . map (take 4) . tails $ loads

        || find the first cycle matching the fingerprint
        go m ((t', fp) : xs)
            = fromMaybef (go m' xs) loadAt (m_lookup (cmplist cmpint) fp m)
              where
                m'       = m_insert (cmplist cmpint) fp t' m
                loadAt t = loads ! (t + (1_000_000_000 - t) $mod (t' - t))
         go _ _ = error "no rocks"

readInput :: string -> io ([rock], loc)
readInput fn
    = (go . lines) <$>. readFile fn
      where
        go rows
            = (rocks, (mx, my))
              where
                my    = #rows
                mx    = #(hd rows)
                rocks = [(c, (x, y)) | (y, r) <- enumerate rows; (x, c) <- enumerate r; c ~=. '.']

day14 :: io ()
day14
    = readInput "../inputs/day14.txt" >>=. go
      where
        go (rocks, limit)
            = io_mapM_ putStrLn [part1, part2]
              where
                part1 = ("part 1: " ++) . showint . load limit . tiltNorth $ rocks
                part2 = ("part 2: " ++) . showint . findCycle limit $ rocks
