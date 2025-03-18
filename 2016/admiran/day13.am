|| day13.m


%export day13

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <dequeue>
%import <maybe>
%import <mirandaExtensions>
%import <vector>
%import <set>

|| perform a popCount 4 bits at a time
popCount4 :: vector int
popCount4 = v_fromList [0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4]

popCount :: int -> int
popCount 0 = 0
popCount n = popCount4 !! (n .&. 0xf) + popCount (n .>>. 4)

point == (int, int)

makeMazeGenerator :: int -> point -> bool
makeMazeGenerator seed
    = go
      where
        go (x, y) = even . popCount $ x * x + 3 * x + 2 * x * y + y + y * y + seed

findPath :: point -> point -> int -> (point -> bool) -> maybe int
findPath start finish stepLimit mazeGen
    = go (dq_singleton (start, 0)) s_empty
      where
        go q visited
            = Nothing,               if dq_null q
            = Just steps,            if _eq cmppoint pt finish
            = Just $ s_size visited, if stepLimit >= 0 & steps > stepLimit
            = go q2 visited',        otherwise
              where
                ((pt, steps), q1) = fromJust $ dq_viewL q
                visited'          = s_insert cmppoint pt visited
                q2                = q1,                              if s_member cmppoint pt visited
                                  = foldr dq_addR q1 $ expand pt, otherwise
                expand (x, y)     = [(p, steps + 1) | p <- [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]; notNeg p & mazeGen p]
                notNeg (x, y)     = x >= 0 & y >= 0

day13 :: io ()
day13
    = io_mapM_ putStrLn [part1, part2]
      where
        seed    = 1362
        mazeGen = makeMazeGenerator seed
        part1   = (++) "part 1: " . showint . fromJust . findPath (1, 1) (31, 39) (-1) $ mazeGen
        part2   = (++) "part 2: " . showint . fromJust . findPath (1, 1) (-1, -1) 50   $ mazeGen
