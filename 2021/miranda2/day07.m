%export day07

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <maybe>
%import <mirandaExtensions>


|| ternary search for min -- f must be a unimodal function
findMin :: (int -> int) -> int -> int -> int
findMin f l r
    = min cmpint [f l, f (l + 1), f r], if r - l < 3
    = findMin f l r',   if f l' < f r'
    = findMin f l' r,   otherwise
      where
        l' = (2 * l + r) $div 3
        r' = (2 * r + l) $div 3

readIntList :: string -> io [int]
readIntList fn = map intval <$>. split ',' <$>. readFile fn

day07 :: io ()
day07
    = readIntList "../inputs/day07.txt" >>=. go
      where
        go crabs
            = io_mapM_ putStrLn [part1, part2]
              where
                part1 = (++) "part 1: " . showint . minCost . moveCost $ cost1
                part2 = (++) "part 2: " . showint . minCost . moveCost $ cost2

                minCost f    = findMin f (min cmpint crabs) (max cmpint crabs)
                moveCost f x = sum (map (f x) crabs)
                cost1 a b    = abs (a - b)
                cost2 a b    = (c * c + c) $div 2 where c = cost1 a b
