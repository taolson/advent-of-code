|| day18.m -- Lavaduct Lagoon


%export day18

%import <io> (>>=.)/io_bind
%import <mirandaExtensions>
%import <v2>


loc == v2 int
dir == v2 int

toDir :: char -> dir
toDir 'U' = V2    0 (-1)
toDir 'D' = V2    0   1
toDir 'L' = V2  (-1)  0
toDir 'R' = V2    1   0
toDir _   = error "toDir: bad direction"

hexval :: string -> int
hexval
    = foldl addDigit 0
      where
        code0    = code '0'
        codea    = code 'a'
        codeA    = code 'A'

        addDigit n c
            = n * 16 + (code c - code0),      if '0' <=. c <=. '9'
            = n * 16 + (code c - codea + 10), if 'a' <=. c <=. 'f'
            = n * 16 + (code c - codeA + 10), if 'A' <=. c <=. 'F'
            = n,                              otherwise


planPart ::= PP dir int int
plan == [planPart]

readPlan1, readPlan2 :: string -> plan
readPlan1
    = map (parse . split ' ') . lines
      where
        parse [dir, cnt, s] = PP (toDir . hd $ dir) (intval cnt) (hexval s)
        parse _             = error "readPlan: bad parse"

readPlan2
    = map (parse . split ' ') . lines
      where
        dirs = "RDLU"
        parse [_, _, s]
            = PP dir cnt hn
              where
                hn  = hexval s
                cnt = hn $div 16
                dir = toDir $ dirs ! (hn $mod 16)

        parse _             = error "readPlan: bad parse"

segment == (loc, loc)

s_start, s_end :: segment -> loc
s_start = fst
s_end   = snd

s_length :: segment -> int
s_length (s, e) = v2_dist s e


|| convert the plan into a list of segments
digTrench :: plan -> [segment]
digTrench
    = snd . mapAccumL go (v2_pure 0)
      where
        smul n = v2_fmap (* n)

        go loc (PP dir cnt rgb)
            = (loc', seg)
              where
                loc'   = v2_add loc $ smul cnt dir
                seg    = (loc, loc')

perimeter :: [segment] -> int
perimeter = sum . map s_length

|| shoelace algorithm for finding the area of a polygon given its vertices
|| A = 1/2 * abs sum [xi yi+1 - xi+1yi]
shoelaceArea :: [segment] -> int
shoelaceArea segs
    = a2 $div 2
      where
        locs  = map s_start segs
        locs' = locs ++ [hd locs]       || append the first onto the end
        a2    = abs . foldl kernel 0 $ zip2 locs' (tl locs')

        kernel s (V2 x1 y1, V2 x2 y2)
            = s + x1 * y2 - x2 * y1

day18 :: io ()
day18
    = readFile "../inputs/day18.txt" >>=. go
      where
        go input
            = io_mapM_ putStrLn [part1, part2]
              where
                segs1     = digTrench . readPlan1 $ input
                segs2     = digTrench . readPlan2 $ input
                process s = shoelaceArea s + perimeter s $div 2 + 1
                part1     = ("part 1: " ++) . showint . process $ segs1 
                part2     = ("part 2: " ++) . showint . process $ segs2
