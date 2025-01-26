|| day11.m


%export day11

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <maybe>
%import <mirandaExtensions>


|| a password is a list of 8 integers (0 .. 25, representing the characters 'a' .. 'z'),
||  stored in reverse order so that the incrementing operation (nominally right-to-left) can
|| proceed in list order
password == [int]

toPassword :: string -> password
toPassword = map ((subtract (code 'a')) . code) . reverse

fromPassword :: password -> string
fromPassword = map (decode . (+ code 'a')) . reverse

mistaken :: password
mistaken = toPassword "iol"

incPassword :: password -> password
incPassword
    = go
      where
        go [] = []
        go (c : cs)
            = 0  : go cs, if c == 25                    || wrap-around increment, continue with incrementing
            = c2 : cs,    if member cmpint mistaken c1  || incremented to bad character; increment it again
            = c1 : cs,    otherwise                     || increment character
              where
                c1 = c + 1
                c2 = c + 2

|| scan a password for a matching predicate test
scanPassword :: (password -> bool) -> password -> bool
scanPassword tst
    = foldr ((\/) . tst) False . tails

|| look for a sequence of 3 consecutive increasing characters
hasStraight :: password -> bool
hasStraight
    = scanPassword straight
      where
        straight (a : b : c : xs) = a == b + 1 & b == c + 1
        straight _ = False

|| look for two non-matching, non-overlapping pairs of characters
hasPairs :: password -> bool
hasPairs
    = scanPassword pair1
      where
        pair1 (a : b : xs) = scanPassword (pair2 a) xs, if a == b
        pair1 _ = False

        pair2 x (a : b : xs) = a == b & a ~= x
        pair2 _ _ = False

|| check for legal password
|| second constraint of not containing letters i, o, or l has already been handled
|| in the increment code, which optimizes skipping over those letters if they are
|| at the end of a chain of wrap-around increments
legal :: password -> bool
legal p = hasStraight p & hasPairs p

nextPassword :: password -> password
nextPassword = fromJust . find legal . tl . iterate incPassword

day11 :: io ()
day11
    = io_mapM_ putStrLn [part1, part2]
      where
        p     = toPassword "hepxcrrq"
        p1    = nextPassword p
        p2    = nextPassword p1
        part1 = (++) "part 1: " . fromPassword $ p1
        part2 = (++) "part 2: " . fromPassword $ p2
