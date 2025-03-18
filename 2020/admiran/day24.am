|| day24.m


%export day24

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <mirandaExtensions>
%import <set>


point == (int, int)

direction ::= NE | E | SE | SW | W | NW

path == [direction]

step :: direction -> point -> point
step d (n, e)
    = case d of
        NE -> (n + 1, e + 1)
        E  -> (n + 0, e + 2)
        SE -> (n - 1, e + 1)
        SW -> (n - 1, e - 1)
        W  -> (n + 0, e - 2)
        NW -> (n + 1, e - 1)

blackGrid == s_set point

flipTile :: point -> blackGrid -> blackGrid
flipTile p blacks
    = s_delete cmppoint p blacks, if s_member cmppoint p blacks
    = s_insert cmppoint p blacks, otherwise

walk :: path -> point
walk = foldr step (0, 0)

adjacent :: point -> [point]
adjacent p = map ($step p) [NE, E, SE, SW, W, NW]

adjacentBlackCount :: blackGrid -> point -> int
adjacentBlackCount blacks p
    = foldr tallyBlack 0 $ adjacent p
      where
        tallyBlack p' n
            = n + 1, if s_member cmppoint p' blacks
            = n,     otherwise

evolve :: blackGrid -> blackGrid
evolve blacks
    = s_union cmppoint blacks' $ s_difference cmppoint blacks whites'
      where
        allAdj      = foldl ins s_empty . concatMap adjacent $ s_toList blacks
        whites      = s_filter cmppoint (notMem blacks) allAdj
        whites'     = s_filter cmppoint (flipWhite . adjacentBlackCount blacks) blacks
        blacks'     = s_filter cmppoint (flipBlack . adjacentBlackCount blacks) whites
        flipWhite n = n == 0 \/ n > 2
        flipBlack n = n == 2
    
        || these functions are saturated versions of what used to be a partially-applied s_insert / s_member with cmppoint
        || the partially-applied functions caused excessive over/under application calls at runtime because the compiler
        || couldn't fix them up
        ins s p    = s_insert cmppoint p s
        notMem s p = ~s_member cmppoint p s


toPath :: string -> path
toPath ('n' : 'e' : cs) = NE : toPath cs
toPath ('e' :       cs) = E  : toPath cs
toPath ('s' : 'e' : cs) = SE : toPath cs
toPath ('s' : 'w' : cs) = SW : toPath cs
toPath ('w' :       cs) = W  : toPath cs
toPath ('n' : 'w' : cs) = NW : toPath cs
toPath _                = []

day24 :: io ()
day24
    = readFile "../inputs/day24.txt" >>=. (go . map toPath . lines)
      where
        go paths
            = io_mapM_ putStrLn [part1, part2]
              where
                blacks  = foldr (flipTile . walk) s_empty paths
                blacks' = hd . drop 100 . iterate evolve $ blacks
                part1   = (++) "part 1: " . showint . s_size $ blacks
                part2   = (++) "part 2: " . showint . s_size $ blacks'
