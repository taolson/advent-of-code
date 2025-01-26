%export day04

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <either>
%import <maybe>
%import <mirandaExtensions>
%import <set>


rowcol == s_set int

|| a bingo board is represented as a list of row/col number sets
board == [rowcol]

|| make a board from a list of 25 numbers
makeBoard :: [int] -> board
makeBoard ns
    = map (s_fromList cmpint) (rows ++ cols)
      where
        rows = chunk 5 ns
        cols = transpose rows

|| mark a bingo board with a number by removing it from any row/col
|| if any subsequent row/col is empty, return the board score, otherwise
|| return the new board
|| the board score is the Left side of the either, since it is the exceptional condition
markBoard :: int -> board -> either int board
markBoard n b
    = Left score, if any s_null b'
    = Right b',   otherwise
      where
        b'    = map (s_delete cmpint n) b
        score = (n * foldl (s_foldl (+)) 0 b') $div 2    || div 2 because we are tallying remaining twice for row / col

|| play a round, either stopping at the first bingo or 
|| removing bingo boards until there is only 1 left
playRound :: bool -> [board] -> int -> either int [board]
playRound firstBingo bs n
    = foldr playBoard (Right []) bs
      where
        playBoard b rest
            = Left s,           if isLeft r & (firstBingo \/ lastBoard) || return the score
            = Right (b' : bs'), if isRight r & isRight rest             || return the updated boards
            = rest,             otherwise                               || remove bingo and continue
              where
                lastBoard = null (tl bs)
                r         = markBoard n b
                Left s    = r
                Right b'  = r
                Right bs' = rest

|| play the boards with a sequence of draws, either stopping at the first bingo or the last bingo
playGame :: bool -> [board] -> [int] -> int
playGame firstBingo bs ns
    = error "no bingo found!", if isRight r
    = s,                       otherwise
      where
        r      = e_foldM (playRound firstBingo) bs ns
        Left s = r

readGame :: string -> io ([int], [board])
readGame fn
    = go <$>. break (==. '\n') <$>. readFile fn
      where
        go (s1, s2) = (p_draw s1, p_boards s2)
        p_draw      = map intval . split ','
        p_boards    = map makeBoard . chunk 25 . map intval . words

day04 :: io ()
day04
    = readGame "../inputs/day04.txt" >>=. go
      where
        go (draws, boards)
            = io_mapM_ putStrLn [part1, part2]
              where
                part1 = (++) "part 1: " . showint $ playGame True  boards draws
                part2 = (++) "part 2: " . showint $ playGame False boards draws
