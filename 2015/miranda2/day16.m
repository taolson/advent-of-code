|| day16.m


%export day16

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <map>
%import <maybe>
%import <mirandaExtensions>


constraint == (string, int)
aunt       == m_map string int

matchConstraints :: (string -> int -> int -> bool) -> [constraint] -> aunt -> bool
matchConstraints f cs a
    = all match cs
      where
        match (k, v) = fromMaybef True (f k v) $ m_lookup cmpstring k a

parseLine :: string -> [constraint]
parseLine
    = map mkConstraint . chunk 2 . splitOneOf cmpchar ":, "
      where
        mkConstraint [k, v] = (k, intval v)
        mkConstraint _      = error "parseLine: bad parse"

readInput :: string -> io [aunt]
readInput fn = map (m_fromList cmpstring . parseLine) <$>. lines <$>. readFile fn

day16 :: io ()
day16
    = readInput "../inputs/day16.input" >>=. go
      where
        cs = parseLine "children: 3, cats: 7, samoyeds: 2, pomeranians: 3, akitas: 0, vizslas: 0, goldfish: 5, trees: 3, cars: 2, perfumes: 1"

        || part 1: all constraint tests are for equality
        pt1 _ n v = v == n

        || part 2: cats and trees compare >, pomeranians and goldfish compare <, all others compare ==
        pt2 s n v
            = v > n,  if s ==$ "cats" \/ s ==$ "trees"
            = v < n,  if s ==$ "pomeranians" \/ s ==$ "goldfish"
            = v == n, otherwise

        go aunts 
            = io_mapM_ putStrLn [part1, part2]
              where
                part1 = (++) "part 1: " $ process pt1
                part2 = (++) "part 1: " $ process pt2

                process f = showint . (+ 1) . fst . hd . filter (matchConstraints f cs . snd) . enumerate $ aunts
