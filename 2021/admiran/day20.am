%export day20

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <maybe>
%import <mirandaExtensions>
%import <set>

pixels    == [[int]]            || a 2-D list of intbers (0 = off, 1 = on)
algorithm == s_set int          || set of 9-bit intbers (0 .. 511) that result in an "on" pixel
image     == (pixels, bool)     || pixels, inversion flag

i_pixels :: image -> pixels
i_pixels = fst

|| a scanner scans a list sequentially, performing a function on 3 consecutive
|| values of the list, with two default padding values at both the beginning and end
scanner * ::= S * * * * [*]

|| initialize with two default padding values at the beginning
sc_init :: * -> [*] -> scanner *
sc_init d []       = error "sc_init: []"
sc_init d (x : xs) = S d d d x xs

sc_scan :: (* -> * -> * -> **) -> scanner * -> [**]
sc_scan f (S d a b c [])       = f a b c : f b c d : [f c d d]          || add final 2 default padding values at end
sc_scan f (S d a b c (x : xs)) = f a b c : sc_scan f (S d b c x xs)     || step the scan one element

|| apply an algorithm to the image by scanning the rows with a scanner; and for
|| each set of 3 rows, scan the corresponding columns
i_apply :: algorithm -> image -> image
i_apply algo (rows, inv)
    = (rows', doInv & ~inv)
      where
        doInv             = s_member cmpint 0 algo
        rows'             = sc_scan doRow (sc_init (repeat 0) rows)
        doRow r1 r2 r3    = go (scanRow 64 r1) (scanRow 8 r2) (scanRow 1 r3)
        scanRow m r       = sc_scan (addPixels m) (sc_init 0 r)
        addPixels m a b c = a * m * 4 + b * m * 2 + c * m
        go (s1 : r1') (s2 : r2') (s3 : r3')
            = p' : go r1' r2' r3'
              where
                idx  = s1 + s2 + s3                     || add up all the pixel weights to form the index
                idx' = idx,       if ~doInv \/ ~inv
                     = 511 - idx, otherwise
                p    = 1,         if s_member cmpint idx' algo || look up the index in the algorithm to see if it should be on
                     = 0,         otherwise
                p'   = p,         if ~doInv \/ inv      || do an inversion, if required
                     = 1 - p,     otherwise
        go r1 r2 r3  = []

readAlgorithm :: string -> algorithm
readAlgorithm s
    = foldl (converse (s_insert cmpint)) s_empty [n | (n, c) <- zip2 [0 ..] s; c ==. '#']

readImage :: string -> image
readImage s
    = (map (map cvtPixel) (lines s), False)
      where
        cvtPixel '#' = 1
        cvtPixel c   = 0

readInput :: string -> io (algorithm, image)
readInput fn
    = go <$>. span (not . isSpace) <$>. readFile fn
      where
        go (as, is)
            = (algo, image)
              where
                algo  = readAlgorithm as
                image = readImage . tl . tl $ is

day20 :: io ()
day20
    = readInput "../inputs/day20.txt" >>=. go
      where
        go (algo, image)
            = io_mapM_ putStrLn [part1, part2]
              where
                part1 = (++) "part 1: " . showint . sum . concat . i_pixels . enhance $ 2
                part2 = (++) "part 2: " . showint . sum . concat . i_pixels . enhance $ 50

                enhance n = iterate (i_apply algo) image ! n        

