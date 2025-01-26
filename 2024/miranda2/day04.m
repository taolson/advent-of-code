|| day04.m -- Ceres Search


%export day04

%import "adventLib"
%import <maybe>
%import <v2>            (|+|)/v2_add (|-|)/v2_sub
%import <vector>


pos      == v2 int
grid     == vector char         || grid as linear vector of char, as it is faster than using a map of positions
gridInfo == (grid, int, int)    || grid, num rows, num cols

(!!?) :: gridInfo -> pos -> maybe char
(g, nr, nc) !!? (V2 x y)
    = Just (v_unsafeIndex g (x + y * nc)), if 0 <= x < nc & 0 <= y < nr || unsafeIndex is ok since we are already checking bounds
    = Nothing,                             otherwise

allGridPositions :: gridInfo -> [pos]
allGridPositions (_, nr, nc) = [V2 x y | x <- [0 .. nc - 1]; y <- [0 .. nr - 1]]

makeGridInfo :: string -> gridInfo
makeGridInfo input
    = (g, nrows, ncols)
      where
        rows  = lines input
        nrows = #rows
        ncols = #hd rows
        g     = concat rows |> v_fromList

|| step from a starting position in the grid in a direction, attempting to match the target string
stepMatch :: gridInfo -> pos -> pos -> string -> bool
stepMatch grid start step s
    = start |> iterate (|+| step) |> zip2 s |> all match
      where
        match (c, p) = fromMaybef False (==. c) $ grid !!? p

|| find the total occurrences of a string on a grid in any direction
occurrences :: gridInfo -> string -> int
occurrences gi s
    = #[p | p <- allGridPositions gi; dx, dy <- [-1 .. 1]; dx ~= 0 \/ dy ~= 0; stepMatch gi p (V2 dx dy) s]

|| find the total occurrences of a string (forming an 'X' shape with itself) on a grid
xoccurrences :: gridInfo -> string -> int
xoccurrences gi s
    = #[p | p <- allGridPositions gi; diagMatch p d1 & diagMatch p d2]
      where
        r  = #s $div 2  || distance to go from the center of the X shape to the start point for matching
        d1 = V2 1 1     || diagonal step 1
        d2 = V2 1 (-1)  || diagonal step 2

        || scalar multiplication of a v2
        n *^ (V2 x y) = V2 (n * x) (n * y)

        diagMatch p d
            = stepMatch gi (p |-| rd) d s \/            || step min to max
              stepMatch gi (p |+| rd) (v2_neg d) s      || step max to min
              where
                rd = r *^ d 

day04 :: io ()
day04
    = readFile "../inputs/day04.txt" >>= makeGridInfo .> go
      where
        go grid
            = output [part1, part2]
              where
                part1 = occurrences grid "XMAS" |> showint
                part2 = xoccurrences grid "MAS" |> showint
