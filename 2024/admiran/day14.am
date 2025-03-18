|| day14.m -- Restroom Redoubt


%export day14

%import "adventLib"
%import <set>
%import <v2>            (|+|)/v2_add


pos   == v2 int
vel   == v2 int
rect  == v2 pos         || origin, < limit
robot == (pos, vel)

lim :: pos
lim = V2 101 103

|| strict to prevent space leak on computing p'
step :: robot -> robot
step (p, v) = ((p |+| v) $v2_mod lim, v)

runRobots :: [robot] -> [[robot]]
runRobots = iterate (map step)

quadrants :: [rect]
quadrants
    = [V2 (V2 x y) (V2 (x + hx) (y + hy)) | x <- [0, hx + 1]; y <- [0, hy + 1]]
      where
        V2 hx hy = lim $v2_div v2_pure 2

(|<=|), (|<|) :: pos -> pos -> bool
V2 a b |<=| V2 c d = a <= c & b <= d
V2 a b |<|  V2 c d = a < c & b < d

inQuadrant :: rect -> robot -> bool
inQuadrant (V2 lo hi) (p, _) = lo |<=| p & p |<| hi

safetyFactor :: [robot] -> int
safetyFactor rs = [count (inQuadrant q) rs | q <- quadrants] |> product
        
makeRobots :: string -> [robot]
makeRobots
    = lines .> map mkRobot
      where
        err      = error "parse error"
        mkRobot  = words .> map mkPosVel .> mkPair
        mkPosVel = drop 2 .> split ',' .> map intval .> mkV2

        mkV2 [x, y]   = V2 x y
        mkV2 _        = err
        mkPair [p, v] = (p, v)
        mkPair _      = err

swapXY :: pos -> pos
swapXY (V2 x y) = V2 y x

showRobotGrid :: [robot] -> string
showRobotGrid
    = map (fst .> swapXY) .> sortBy cmppos .> go 0 0
      where
        go x y [] = "\n"
        go x y rs
            = '\n' : go (x + 1) 0 rs,  if x < x'
            = ' '  : go x (y + 1) rs,  if y < y'
            = '*'  : go x (y + 1) rs', otherwise
              where
                V2 x' y' : rs' = rs

|| the idea here is that a tree image is likely made up of a tight grouping of a large number of the robots, so we
|| run through the possible confgurations (must be modulo (lim x * lim y)) and pick the one with the smallest safetyFactor
findTree :: [robot] -> int
findTree = runRobots .> map safetyFactor .> take (v2_product lim) .> enumerate .> minBy cmpint snd .> fst

day14 :: io ()
day14
    = readFile "../inputs/day14.txt" >>= makeRobots .> go
      where
        go robots
            = output [part1, part2]
              where
                part1 = runRobots robots ! 100 |> safetyFactor |> showint
                part2 = findTree robots |> showint
