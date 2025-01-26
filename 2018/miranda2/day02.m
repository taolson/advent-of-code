|| day02.m


%export day02

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <map>
%import <mirandaExtensions>


boxId ::= BoxId string [int]

bid :: boxId -> string
bid (BoxId s ns) = s

letterCounts :: boxId -> [int]
letterCounts (BoxId s ns) = ns

parseId :: string -> boxId
parseId s
    = BoxId s counts
      where
        counts  = m_elems . foldr ins m_empty $ s
        ins c b = m_insertWith cmpchar (+) c 1 b

matchLetterCount :: int -> [boxId] -> [boxId]
matchLetterCount n = filter (any (== n) . letterCounts)

distance :: boxId -> boxId -> int
distance b1 b2 = length . filter not $ zipWith (==.) (bid b1) (bid b2)

common :: ordI * -> [*] -> [*] -> [*]
common cmp l1 l2 = map fst . filter (uncurry (_eq cmp)) $ zip2 l1 l2

allPairs :: [*] -> [(*, *)]
allPairs [] = []
allPairs (x : xs) = map ($pair x) xs ++ allPairs xs


day02 :: io ()
day02
    = readFile "../inputs/day02.input" >>=. (go . map parseId . lines)
      where
        go input
            = io_mapM_ putStrLn [part1, part2]
              where
                match2      = matchLetterCount 2 input
                match3      = matchLetterCount 3 input
                closestPair = hd . filter ((1 ==) . uncurry distance) . allPairs $ input
                part1 = (++) "part 1: " . showint $ #match2 * #match3
                part2 = (++) "part 2: " . showstring . uncurry (common cmpchar) . mapBoth bid $ closestPair
