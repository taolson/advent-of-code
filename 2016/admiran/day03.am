|| day03.m


%export day03

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <mirandaExtensions>


triangle == (int, int, int)

toTriangles :: [int] -> [triangle]
toTriangles
    = map toTriangle . chunk 3
      where
        toTriangle [a, b, c] = (a, b, c)
        toTriangle xs         = error "toTriangles: bad input"

validTriangle :: triangle -> bool
validTriangle (a, b, c) = a + b > c & b + c > a & c + a > b

remap :: [triangle] -> [triangle]
remap ts
    = toTriangles $ tsA ++ tsB ++ tsC
      where
        (tsA, tsB, tsC) = unzip3 ts

readInput :: string -> io [triangle]
readInput fn = toTriangles <$>. map intval <$>. words <$>. readFile fn

day03 :: io ()
day03
    = readInput "../inputs/day03.input" >>=. go
      where
        go tris
            = io_mapM_ putStrLn [part1, part2]
              where
                process = showint . length . filter validTriangle
                part1   = (++) "part 1: " . process $ tris
                part2   = (++) "part 2: " . process . remap $ tris
