|| day05.m


%export day05

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <mirandaExtensions>


badPairs :: [string]
badPairs  = ["ab", "cd", "pq", "xy"]

nice1 :: string -> bool
nice1
    = go (decode 0) 0 0
      where
        isVowel = member cmpchar "aeiou"

        go prev vowelCount dupCount [] = vowelCount >= 3 & dupCount > 0
        go prev vowelCount dupCount (c : rest)
            = False,                           if member cmpstring badPairs [prev, c]
            = go c vowelCount' dupCount' rest, otherwise
              where
                vowelCount' = if' (isVowel c)  (vowelCount + 1) vowelCount
                dupCount'   = if' (c ==. prev) (dupCount + 1)   dupCount


cpair == (char, char, int)

nice2 :: string -> bool
nice2
    = go (decode 0) (decode 0) 0 [] 0 0
      where
        go prev2 prev1 idx pairs pairCount dupCount [] = dupCount > 0 & pairCount > 0
        go prev2 prev1 idx pairs pairCount dupCount (c : rest)
            = go prev1 c (idx + 1) pairs' pairCount' dupCount' rest
              where
                pairs'      = currentPair : pairs
                pairCount'  = if' (matchPair currentPair pairs) (pairCount + 1) pairCount
                dupCount'   = if' (c ==. prev2) (dupCount + 1) dupCount
                currentPair = (prev1, c, idx)
                matchPair _ [] = False
                matchPair p ((p1, p2, j) : rest)
                    = True,             if c1 ==. p1 & c2 ==. p2 & i > j + 1
                    = matchPair p rest, otherwise
                      where
                        (c1, c2, i) = p
                matchPair p _ = undef   || added to remove compiler warning

day05 :: io ()
day05
    = readFile "../inputs/day05.input" >>=. (go . lines)
      where
        go input
            = io_mapM_ putStrLn [part1, part2]
              where
                part1 = (++) "part 1: " . showint . length . filter nice1 $ input
                part2 = (++) "part 2: " . showint . length . filter nice2 $ input
