%export day10

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <map>
%import <maybe>
%import <mirandaExtensions>

parseStatus ::= Legal | Incomplete | Corrupted
parseState  ==  (string, string)                || completion stack, input stream
parseResult ==  (parseStatus, parseState)
parser      ==  parseState -> parseResult

getParseStatus :: parseResult -> parseStatus
getParseStatus (ps, st) = ps

isLegal, isIncomplete, isCorrupted  :: parseStatus -> bool
isLegal Legal           = True
isLegal ps              = False
isIncomplete Incomplete = True
isIncomplete ps         = False
isCorrupted Corrupted   = True
isCorrupted ps          = False

closingDelims :: m_map char char
closingDelims
    = (m_fromList cmpchar . map mkPair) ["()", "[]", "{}", "<>"]
      where
        mkPair [a, b] = (a, b)
        mkPair xs     = undef   || dummy default to remove non-exhaustive pattern error

(>>) :: parseResult -> parser -> parseResult
pr >> p
    = p st, if isLegal ps
    = pr,   otherwise
      where
        (ps, st) = pr

p_chunk :: parser
p_chunk st
    = (Legal, st),               if  null cs &  null ds
    = (Incomplete, st),          if  null cs & ~null ds
    = (Legal, stDone),           if ~null ds & c ==. d'
    = (Corrupted, st),           if isNothing md
    = p_chunk stMore >> p_chunk, otherwise
      where
        (ds, cs) = st
        c  : cs' = cs
        d' : ds' = ds
        md       = m_lookup cmpchar c closingDelims
        d        = fromJust md
        stDone   = (ds', cs')
        stMore   = (d : ds, cs')

parse :: string -> parseResult
parse s = p_chunk ([], s)

corruptScore :: parseResult -> int
corruptScore (Corrupted, (ds, c : cs))
    = 3,     if c ==. ')'
    = 57,    if c ==. ']'
    = 1197,  if c ==. '}'
    = 25137, if c ==. '>'
corruptScore pr = 0

incompleteScore :: parseResult -> int
incompleteScore (Incomplete, (ds, cs))
    = foldl tally 0 ds
      where
        tally n d = error ("bad delimiter: " ++ [d]), if isNothing mi
                  = n * 5 + i + 1,                    otherwise
                    where
                      mi = elemIndex cmpchar d ")]}>"
                      i  = fromJust mi

incompleteScore pr = 0

median :: [int] -> int
median = hd . snd . split2 . sortBy cmpint

readParsedChunks :: string -> io [(parseStatus, parseState)]
readParsedChunks fn
    = map parse <$>. lines <$>. readFile fn

day10 :: io ()
day10
    = readParsedChunks "../inputs/day10.txt" >>=. go
      where
        go parsed
            = io_mapM_ putStrLn [part1, part2]
              where
                part1  = (++) "part 1: " . showint . sum . map corruptScore . filter (isCorrupted . getParseStatus) $ parsed
                part2  = (++) "part 2: " . showint . median . map incompleteScore . filter (isIncomplete . getParseStatus) $ parsed
