|| day20.m -- Race Condition


%export day20

%import "adventLib"
%import <maybe>         (<$>?)/mb_fmap (<|>?)/mb_alt
%import <set>
%import <v2>            (|+|)/v2_add
%import <vector>


pos == v2 int
dir == v2 int

dirs :: [dir]
dirs = [V2 (-1) 0, V2 0 1, V2 1 0, V2 0 (-1)]

maze     == vector char
mazeInfo == (maze, int, pos, pos)       || maze, width, start, end
path     == vector pos

makeMazeInfo :: string -> mazeInfo
makeMazeInfo s
    = (vm, width, start, end)
      where
        rows  = lines s
        width = #(hd rows)
        vm    = concat rows |> v_fromList
        info  = [(V2 r c, x) | (r, row) <- enumerate rows; (c, x) <- enumerate row; letter x]

        [(end, _), (start, _)] = sortBy (comparing cmpchar snd) info

|| return the path through the maze from the start to the end
|| this can just be a simple DFS, since there is only one path through this maze
mazePath :: mazeInfo -> path
mazePath (vm, width, start, end)
    = dfs s_empty start |> fromJust |> v_fromList
      where
        dfs seen p
            = Just [end],                if _eq cmppos p end
            = Nothing,                   if s_member cmppos p seen
            = foldr tryNext Nothing exp, otherwise
              where
                seen' = s_insert cmppos p seen
                exp   = [p' | d <- dirs; p' <- [p |+| d]; _ne cmppos p' p; mazeAt p' ~=. '#']

                mazeAt (V2 r c) = vm !! (r * width + c)
                tryNext p' k    = ((p :) <$>? dfs seen' p') <|>? k
                        
|| find the number of unique cheats possible within the specified cheat times that save at least save steps
|| this trick was mentioned in the advent of code subreddit: instead of trying to scan a manhattan-distance
|| diamond around the current location for path positions that save at least the required number of steps,
|| simply start another stepper ahead on the path by the required savings, and then check the manhattan distance
|| between the start and end.
findCheats :: path -> int -> (int, int) -> (int, int)
findCheats path save (ta, tb)
    = foldl go1 (0, 0) [0 .. end]
      where
        end = v_length path - 1
        
        go1 cheats i
            = foldl go2 cheats [i + save .. end]        || start looking save steps ahead on the path
              where
                p1 = v_unsafeIndex path i               || p1 is the starting point for our possible cheat
                go2 cheats j
                    = addCheat cheats
                      where
                        p2 = v_unsafeIndex path j       || p2 is the ending point for our possible cheat
                        dm = v2_dist p1 p2              || dm is the manhattan distance of the possible cheat
                        dp = j - i                      || dp is the path distance of the possible cheat

                        || uses strict addition to prevent space leaks 
                        addCheat (a, b)
                            = case a + da of a' -> case b + db of b' -> (a', b')
                              where
                                da = if' (dm <= ta & dp - dm >= save) 1 0       || valid cheat if manhattan dist <= cheat time & saved steps >= required save
                                db = if' (dm <= tb & dp - dm >= save) 1 0

day20 :: io ()
day20
    = readFile "../inputs/day20.txt" >>= makeMazeInfo .> go
      where
        go mazeInfo
            = output [part1, part2]
              where
                path     = mazePath mazeInfo
                cheats   = findCheats path 100 (2, 20)
                part1    = fst cheats |> showint
                part2    = snd cheats |> showint
