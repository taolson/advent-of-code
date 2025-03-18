|| day17.m


%export day17

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <dequeue>
%import <maybe>
%import <mirandaExtensions>
%import "md5"


room == (int, int)

direction ::= U | D | L | R

mazeState == (room, string)

moveDirection :: mazeState -> direction -> mazeState
moveDirection ((x, y), path) d
    = case d of
        U -> ((x, y - 1), path ++ "U")
        D -> ((x, y + 1), path ++ "D")
        L -> ((x - 1, y), path ++ "L")
        R -> ((x + 1, y), path ++ "R")

movesFrom :: string -> mazeState -> [mazeState]
movesFrom passcode state
    = filter validRoom . map (moveDirection state) $ validDirections, otherwise
      where
        path            = snd state
        digest          = take 4 . md5Hex . md5Hash $ passcode ++ path
        validDirections = map snd . filter openDoor $ zip2 digest [U, D, L, R]

        validRoom ((x, y), _) = 0 <= x < 4 & 0 <= y < 4
        openDoor (c, _)       = member cmpchar "bcdef" c

findPath :: string -> room -> room -> (string, string)
findPath passcode start finish
    = findPath' (dq_singleton (start, "")) []
      where
        findPath' q solutions
            = (minSol, maxSol),                if dq_null q
            = findPath' q1 (path : solutions), if _eq cmproom room finish
            = findPath' q2 solutions,          otherwise
              where
                (state, q1)  = fromJust $ dq_viewL q
                (room, path) = state
                minSol       = minBy cmpint length solutions
                maxSol       = maxBy cmpint length solutions
                q2           = foldl (converse dq_addR) q1 $ movesFrom passcode state

day17 :: io ()
day17
    = io_mapM_ putStrLn [part1, part2]
      where
        (minp, maxp) = findPath "pvhmgsws" (0, 0) (3, 3)
        part1 = "part 1: " ++ minp
        part2 = "part 2: " ++ showint (#maxp)
