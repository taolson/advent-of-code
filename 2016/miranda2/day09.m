|| day09.m


%export day09

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <mirandaExtensions>


decompress1 :: string -> int
decompress1 [] = 0
decompress1 (c : rest)
    = processed + decompress1 rest', if c ==. '('
    = decompress1 rest,              if isSpace c
    = 1 + decompress1 rest,          otherwise
      where
        (processed, rest') = processMarker1 rest

processMarker1 :: string -> (int, string)
processMarker1 s
    = (rep * count, rest)
      where
        (count, (_ : s1)) = readInt s
        (rep, (_ : s2))   = readInt s1
        rest              = drop count s2

readInt :: string -> (int, string)
readInt
    = go 0
      where
        go n [] = (n, [])
        go n s
            = go (n * 10 + digitVal c) s', if digit c
            = (n, s),                      otherwise
              where
                c : s' = s

decompress2 :: string -> int
decompress2 [] = 0
decompress2 (c : rest)
    = processed + decompress2 rest', if c ==. '('
    = decompress2 rest,              if isSpace c
    = 1 + decompress2 rest,          otherwise
      where
        (processed, rest') = processMarker2 rest

processMarker2 :: string -> (int, string)
processMarker2 s
    = (rep * decompress2 pattern, rest)
      where
        (count, (_ : s1)) = readInt s
        (rep, (_ : s2))   = readInt s1
        (pattern, rest)   = splitAt count s2

day09 :: io ()
day09
    = readFile "../inputs/day09.input" >>=. go
      where
        go input
            = io_mapM_ putStrLn [part1, part2]
              where
                part1 = (++) "part 1: " . showint . decompress1 $ input
                part2 = (++) "part 2: " . showint . decompress2 $ input

