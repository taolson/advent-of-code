|| day18.m -- RAM run


%export day18

%import "adventLib"
%import <heap>
%import <maybe>
%import <set>
%import <v2>            (|+|)/v2_add

pos == v2 int
dir == v2 int

dirs :: [dir]
dirs = [V2 (-1) 0, V2 0 1, V2 1 0, V2 0 (-1)]

makeBytes :: string -> [pos]
makeBytes
    = lines .> map (split ',' .> map intval .> mkByte)
      where
        mkByte [x, y] = V2 x y
        mkByte _      = error "parse error"

memSpace == s_set pos

makeMemSpace :: [pos] -> memSpace
makeMemSpace = s_fromList cmppos

solve :: memSpace -> maybe int
solve ms
    = go s_empty (h_singleton (0, V2 0 0))
      where
        cmp  = comparing cmpint fst
        lim  = 70
        end  = v2_pure lim

        go seen q = h_viewMin cmp q |> check seen
              
        check seen Nothing = Nothing
        check seen (Just ((steps, p), q1))
            = Just steps,  if _eq cmppos p end
            = go seen  q1, if s_member cmppos p seen
            = go seen' q2, otherwise
              where
                seen'   = s_insert cmppos p seen
                ins q x = h_insert cmp x q
                q2      = foldl ins q1 expand
                expand  = [(steps + 1, p') | d <- dirs; p' <- [p |+| d]; inBounds p'; ~s_member cmppos p' ms]

                inBounds (V2 x y) = 0 <= x <= lim & 0 <= y <= lim

|| binary search for the lowest value that returns True for the predicate
binSearch :: (int -> bool) -> int -> int
binSearch f max
    = go 0 max (f 0) (f max)
      where
        go lo hi fl fh
            = hi, if hi - lo < 2
            = go lo mid fl fm, if fm
            = go mid hi fm fh, otherwise
              where
                mid = (lo + hi) $div 2
                fm  = f mid

day18 :: io ()
day18
    = readFile "../inputs/day18.txt" >>= makeBytes .> go
      where
        go bytes
            = output [part1, part2]
              where
                process n = take n bytes |> makeMemSpace |> solve
                part1     = process 1024 |> fromJust |> showint
                part2     = binSearch (process .> isNothing) (#bytes) |> getBytePos |> showres

                || the byte info for the Nth block is at n - 1
                getBytePos n     = bytes ! (n - 1)
                showres (V2 x y) = showint x ++ "," ++ showint y
