%export day03

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <maybe>
%import <mirandaExtensions>
%import <set>


rucksack == (s_set char, s_set char)

(<**>) = s_intersect cmpchar
(<++>) = s_union cmpchar

commonItem :: rucksack -> char
commonItem  (c1, c2) = s_first (c1 <**> c2)

allContents :: rucksack -> s_set char
allContents (c1, c2) = c1 <++> c2

badge :: [rucksack] -> char
badge = s_first . foldl1 (<**>) . map allContents

priority :: char -> int
priority c
    = code c - code 'A' + 27, if isUpper c
    = code c - code 'a' + 1,  otherwise

readRucksacks :: string -> io [rucksack]
readRucksacks fn
    = map (mapBoth (s_fromList cmpchar) . split2) <$>. lines <$>. readFile fn

day03 :: io ()
day03
    = readRucksacks "../inputs/day03.txt" >>=. go
      where
        go sacks
            = io_mapM_ putStrLn [part1, part2]
              where
                groups = chunk 3 sacks
                part1  = (++) "part 1: " . showint . sum . map (priority . commonItem) $ sacks
                part2  = (++) "part 2: " . showint . sum . map (priority . badge) $ groups
