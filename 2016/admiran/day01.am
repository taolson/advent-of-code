|| day01.m


%export day01

%import <io>
%import <mirandaExtensions>
%import <set>

point == (int, int)

direction ::= N | E | S | W

step :: direction -> point -> point
step d (x, y)
    = case d of
        N -> (x, y+1)
        E -> (x+1, y)
        S -> (x, y-1)
        W -> (x-1, y)

succWrap :: direction -> direction
succWrap d
    = case d of
        N -> E
        E -> S
        S -> W
        W -> N

predWrap :: direction -> direction
predWrap d
    = case d of
        N -> W
        E -> N
        S -> E
        W -> S

turn ::= L | R

doTurn :: direction -> turn -> direction
dir $doTurn t
    = case t of
        L -> predWrap dir
        R -> succWrap dir

command  == (turn, int)
location == (point, direction)

genSteps :: location -> int -> ([point], location)
genSteps loc n
    = ([], loc),            if n == 0
    = (pt : rest, lastLoc), otherwise
      where
        (pt, dir)       = loc
        (rest, lastLoc) = genSteps (step dir pt, dir) (n - 1)

genPath :: location -> [command] -> ([point], location)
genPath loc cmds
    = ([pt], loc),                     if null cmds
    = (stepPath ++ restPath, lastLoc), otherwise
      where
        (pt, dir)           = loc
        (t, dist)           = hd cmds
        loc'                = (pt, dir $doTurn t)
        (stepPath, stepLoc) = genSteps loc' dist
        (restPath, lastLoc) = genPath stepLoc (tl cmds)
    
manhattanDistance :: point -> int
manhattanDistance (x, y) = abs x + abs y

secondVisit :: s_set point -> [point] -> point
secondVisit s []  = error "secondVisit: empty path"
secondVisit s [p] = p
secondVisit s (p : rest)
    = p,                                        if s_member cmppoint p s
    = secondVisit (s_insert cmppoint p s) rest, otherwise

parseCommands :: string -> [command]
parseCommands
    = map parseCommand . splitOneOf cmpchar ", \n\t"
      where
        parseCommand []       = error "parseCommand: empty line"
        parseCommand (t : ns) = (turnFrom t, intval ns)
        turnFrom 'L' = L
        turnFrom 'R' = R
        turnFrom _   = error "turnFrom: bad char"

day01 :: io ()
day01
    = io_mapM_ putStrLn [part1, part2]
      where
        input =
          "R3, L5, R2, L2, R1, L3, R1, R3, L4, R3, L1, L1, R1, L3, R2, L3, L2, R1, R1, L1, R4, L1, L4, R3,     \
          \L2, L2, R1, L1, R5, R4, R2, L5, L2, R5, R5, L2, R3, R1, R1, L3, R1, L4, L4, L190, L5, L2, R4, L5,   \
          \R4, R5, L4, R1, R2, L5, R50, L2, R1, R73, R1, L2, R191, R2, L4, R1, L5, L5, R5, L3, L5, L4, R4, R5, \
          \L4, R4, R4, R5, L2, L5, R3, L4, L4, L5, R2, R2, R2, R4, L3, R4, R5, L3, R5, L2, R3, L1, R2, R2, L3, \
          \L1, R5, L3, L5, R2, R4, R1, L1, L5, R3, R2, L3, L4, L5, L1, R3, L5, L2, R2, L3, L4, L1, R1, R4, R2, \
          \R2, R4, R2, R2, L3, L3, L4, R4, L4, L4, R1, L4, L4, R1, L2, R5, R2, R3, R3, L2, L5, R3, L3, R5, L2, \
          \R3, R2, L4, L3, L1, R2, L2, L3, L5, R3, L1, L3, L4, L3"

        (path, (finalPt, _)) = genPath ((0, 0), N) . parseCommands $ input
        
        part1 = (++) "part 1: " . showint . manhattanDistance $ finalPt
        part2 = (++) "part 2: " . showint . manhattanDistance . secondVisit s_empty $ path
