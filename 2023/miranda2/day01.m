|| day01.m -- Trebuchet?!


%export day01

%import <io> (>>=.)/io_bind
%import <maybe> (>>=)/mb_bind (<|>)/mb_alt
%import <mirandaExtensions>


calibration == (maybe char, maybe char)

|| extract first and last digits from a string
extractCalibration :: string -> calibration
extractCalibration
    = foldl go (Nothing, Nothing)
      where
        go (fst, lst) c
            = (fst <|> Just c, Just c), if digit c
            = (fst, lst),               otherwise

calibrationValue :: calibration -> int
calibrationValue (Just a, Just b) = digitVal a * 10 + digitVal b
calibrationValue _ = 0

digitWords :: [(string, char)]
digitWords
    = zip2 ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] "123456789"

|| match the beginning of a string with a list of prefixes and substitutions,
|| returning the first substitution that matches
match :: string -> [(string, char)] -> maybe char
match s xs
    = find (prefixes s . fst) xs >>= (Just . snd)
      where
        prefixes s t = isPrefixOf cmpchar t s

|| extract the first and last digits or words that represent digits from a string
extractWordCalibration :: string -> calibration
extractWordCalibration
    = foldl go (Nothing, Nothing) . tails
      where
        mds []       = Nothing
        mds (c : cs) = Just c, if digit c
        mds s        = match s digitWords

        go (fst, lst) s = (fst <|> mdc, mdc <|> lst) where mdc = mds s

day01 :: io ()
day01
    = readFile "../inputs/day01.txt" >>=. go
      where
        go input
            = io_mapM_ putStrLn [part1, part2]
              where
                process f = showint . sum . map calibrationValue . map f . lines $ input
                part1 = ("part 1: " ++) . process $ extractCalibration
                part2 = ("part 2: " ++) . process $ extractWordCalibration
