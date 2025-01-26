|| day10.m -- Hoof It


%export day10

%import "adventLib"
%import <map>
%import <maybe>
%import <v2>            (|+|)/v2_add

|| hiking trail is 0 .. 9 monotonically increasing by 1 each step
|| score is # of 9-height positions reachable from a trailhead (0)

pos      == v2 int
trail    == [pos]


|| fun way to generate the 4 direction vectors!
dirs :: [pos]
dirs = v2_unit |> iterate v2_rot |> take 4
       where
         v2_unit         = V2 1 0
         v2_rot (V2 a b) = V2 b (-a)

grid     == m_map pos int
gridInfo == (grid, pos, [pos])  || grid, max limit, trailheads

trailHeads :: gridInfo -> [pos]
trailHeads (_, _, hs) = hs

makeGridInfo :: string -> gridInfo
makeGridInfo s
    = (grid, V2 nrows ncols, heads)
      where
        rows  = lines s
        nrows = #rows
        ncols = #hd rows
        info  = [(V2 r c, digitVal d) | (r, row) <- enumerate rows; (c, d) <- enumerate row; '0' <=. d <=. '9']
        grid  = m_fromList cmppos info
        heads = info |> filter (snd .> (== 0)) |> map fst

|| walk trails from a starting position, reporting the end point of all reachable trails
expand :: gridInfo -> pos -> [pos]
expand (g, V2 mr mc, _) p
    = go p 0
      where
        go p 9 = [p]
        go p n = [t | p' <- map (p |+|) dirs; check p' n'; t <- go p' n'] where n' = n + 1

        check (V2 r c) n = 0 <= r < mr & 0 <= c < mc & fromMaybef False (== n) (m_lookup cmppos (V2 r c) g)

scores :: gridInfo -> int
scores gi = trailHeads gi |> map (expand gi .> nub cmppos .> length) |> sum

trails :: gridInfo -> int
trails gi = #[t | h <- trailHeads gi; t <- expand gi h]

day10 :: io ()
day10
    = readFile "../inputs/day10.txt" >>= makeGridInfo .> go
      where
        go gi
            = output [part1, part2]
              where
                part1 = scores gi |> showint
                part2 = trails gi |> showint
