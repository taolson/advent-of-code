|| day25.m


%export day25

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <maybe>
%import <mirandaExtensions>

v4 ::= V4 int int int int

v4_distance :: v4 -> v4 -> int
v4_distance (V4 a b c d) (V4 e f g h) = abs (a - e) + abs (b - f) + abs (c - g) + abs (d - h)

makeConstellations :: [v4] -> [[v4]]
makeConstellations
    = foldl add []
      where
        add cs v
            = ((v : concat near) : far)         || collect v and any constellations that have a member within 3 of it into a new constellation
              where
                (near, far) = partition (any ((<= 3) . v4_distance v)) cs

readVecs :: string -> io [v4]
readVecs fn
    = map mkVec <$>. lines <$>. readFile fn
      where
        mkVec = check . map numval . split ','

        check [a, b, c, d] = V4 a b c d
        check _            = error "parse error"

day25 :: io ()
day25
    = readVecs "../inputs/day25.input" >>=.
      (putStrLn . (++) "part 1: " . showint . length . makeConstellations)
