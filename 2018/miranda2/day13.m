|| day13.m


%export day13

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <map>
%import <maybe>
%import <mirandaExtensions>


loc        == (int, int)
dir        == (int, int)
segment    == char
segmentMap == m_map loc segment

isCurve :: segment -> bool
isCurve '/'  = True
isCurve '\\' = True
isCurve _    = False

isTurn :: segment -> bool
isTurn '+' = True
isTurn _   = False
   
segmentDirection :: segment -> dir
segmentDirection '^' = ( 0, -1)
segmentDirection 'v' = ( 0,  1)
segmentDirection '<' = (-1,  0)
segmentDirection '>' = ( 1,  0)
segmentDirection _   = undef

trackForDir :: segment -> segment
trackForDir '^' = '|'
trackForDir 'v' = '|'
trackForDir '<' = '-'
trackForDir '>' = '-'
trackForDir _   = undef

turnLeft :: dir -> dir
turnLeft (dx, dy) = (dy , -dx)

turnRight :: dir -> dir
turnRight (dx, dy) = (-dy, dx)

cart ::= Cart loc dir int int

cartLoc :: cart -> loc
cartLoc (Cart loc _ _ _) = loc

cartId :: cart -> int
cartId (Cart _ _ _ id) = id


track ::= Track segmentMap [cart] [cart] int

trackCarts, trackCrashed :: track -> [cart]
trackCarts   (Track _ cs _ _) = cs
trackCrashed (Track _ _ cs _) = cs

moveCart :: cart -> segmentMap -> cart
moveCart (Cart loc dir turns id) segs
    = Cart loc' dirTurn ((turns + 1) $mod 3) id, if isTurn s
    = Cart loc' dirCurve turns id,               if isCurve s
    = Cart loc' dir turns id,                    otherwise
      where
        (x, y)   = loc
        (dx, dy) = dir
        loc'     = (x+dx, y+dy)
        s        = m_findWithDefault cmploc ' ' loc' segs

        dirTurn
            = turnLeft dir,  if turns == 0
            = turnRight dir, if turns == 2
            = dir,           otherwise

        dirCurve
            = turnLeft dir,  if dy == 0 & s ==. '/'
            = turnLeft dir,  if dy ~= 0 & s ~=. '/'
            = turnRight dir, otherwise

scanOrder :: cart -> cart -> ordering
scanOrder cart1 cart2
    = LT,           if y1 < y2
    = GT,           if y1 > y2
    = cmpint x1 x2, otherwise
      where
        (x1, y1) = cartLoc cart1
        (x2, y2) = cartLoc cart2

tick :: track -> track
tick (Track segs carts _ ticks)
    = Track segs moved crashed (ticks + 1)
      where
        (moved, crashed)            = moveCarts (sortBy scanOrder carts) [] []
        moveCarts [] moved crashed  = (moved, crashed)
        moveCarts (c : cs) moved crashed
            = moveCarts cs' moved' (c' : csCrash ++ movedCrash ++ crashed), if hadCrash
            = moveCarts cs (c' : moved) crashed,                            otherwise
              where
                c'                   = moveCart c segs
                crashTest cart       = cartId cart ~= cartId c' & _eq cmploc (cartLoc cart) (cartLoc c')
                (csCrash, cs')       = partition crashTest cs
                (movedCrash, moved') = partition crashTest moved
                hadCrash             = not . null $ csCrash ++ movedCrash

readTrack :: [string] -> track
readTrack rows
    = Track segs carts [] 0
      where
        (segs, carts) = foldl readRow (m_empty, []) $ enumerate rows

        readRow (segs, carts) (y, row)
            = foldl (readElt y) (segs, carts) $ enumerate row

        readElt y (segs, carts) (x, ch)
            = (m_insert cmploc (x, y) ch segs, carts), if member cmpchar "|-+/\\" ch
            = (m_insert cmploc (x, y) (trackForDir ch) segs, (Cart (x, y) (segmentDirection ch) 0 (length carts)) : carts), if member cmpchar "^v<>" ch
            = (segs, carts), otherwise

day13 :: io ()
day13
    = readFile "../inputs/day13.input" >>=. (go . readTrack . lines)
      where
        go track
            = io_mapM_ putStrLn [part1, part2]
              where
                track1 = hd . dropWhile (null . trackCrashed) . iterate tick $ track
                track2 = hd . dropWhile ((> 1) . length . trackCarts) . iterate tick $ track1
                part1  = (++) "part 1: " . showloc . cartLoc . hd . trackCrashed $ track1
                part2  = (++) "part 2: " . showloc . cartLoc . hd . trackCarts   $ track2

