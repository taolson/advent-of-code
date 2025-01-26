|| day08.m


%export day08

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <maybe>
%import <mirandaExtensions>
%import <parser> (<*)/p_left (*>)/p_right (<|>)/p_alt
%import <state> (>>=)/st_bind (<$>)/st_fmap (>>)/st_right
%import <vector>


screen  ::= Screen  int int (vector int)        || cols, rows, pixels
mscreen ::= MScreen int int (mvector int)       || mutable version of screen

initScreen :: int -> int -> screen
initScreen cols rows = Screen cols rows $ v_rep (rows * cols) 0

showscreen :: screen -> string
showscreen (Screen c r v)
    = go 0
      where
        limit = r * c
        go idx
            = [],                     if idx == limit
            = ch : sp : go (idx + 1), otherwise
              where
                ch = if' (v !! idx == 1)       '#'  '.'
                sp = if' (idx $mod c == c - 1) '\n' ' '

pixelsLit :: screen -> int
pixelsLit (Screen c r v) = v_sum v

thawScreen :: screen -> mscreen
thawScreen (Screen c r v) = MScreen c r $ v_unsafeThaw v

freezeScreen :: mscreen -> st screen
freezeScreen (MScreen c r mv) = Screen c r <$> v_unsafeFreeze mv

rotateRow :: int -> int -> mscreen -> st ()
rotateRow row count (MScreen c r mv)
    = st_bind2 doFront doBack merge
      where
        base    = row * c
        n       = count $mod c
        doFront = st_mapM (v_unsafeRead mv) [base + i | i <- [c - n .. c - 1]]
        doBack  = st_mapM (v_unsafeRead mv) [base + i | i <- [0 .. c - n - 1]]
        
        merge front back
            = v_unsafeReplace mv . zip2 [base + i | i <- [0 .. c - 1]] $ front ++ back

rotateCol :: int -> int -> mscreen -> st ()
rotateCol col count (MScreen c r mv)
    = st_bind2 doFront doBack merge
      where
        base    = col
        n       = count $mod r
        doFront = st_mapM (v_unsafeRead mv) [base + i * c | i <- [r - n .. r - 1]]
        doBack  = st_mapM (v_unsafeRead mv) [base + i * c | i <- [0 .. r - n - 1]]

        merge front back
            = v_unsafeReplace mv . zip2 [base + i * c | i <- [0 .. r - 1]] $ front ++ back

fillRect :: int -> int -> mscreen -> st ()
fillRect width height (MScreen c r mv)
    = v_unsafeReplace mv [(y * c + x, 1) | y <- [0 .. height - 1]; x <- [0 .. width - 1]]


command ::= Fill int int | RRow int int | RCol int int

processCommands :: [command] -> screen -> screen
processCommands cmds scr
    = st_evalState (st_mapM_ doCmd cmds >> freezeScreen mscr) ()
      where
        mscr = thawScreen scr

        doCmd (Fill w h) = fillRect  w h mscr
        doCmd (RRow r n) = rotateRow r n mscr
        doCmd (RCol c n) = rotateCol c n mscr

p_row, p_col, p_rot, p_fill, p_cmd :: parser command
p_row  = p_liftA2 RRow (p_string "row y="    *> p_int) (p_string " by " *> p_int)
p_col  = p_liftA2 RCol (p_string "column x=" *> p_int) (p_string " by " *> p_int)
p_rot  = p_string "rotate " *> (p_row <|> p_col)
p_fill = p_liftA2 Fill (p_string "rect " *> p_int) (p_char 'x' *> p_int)
p_cmd  = (p_rot <|> p_fill) <* p_spaces

readCommands :: string -> io [command]
readCommands fn
    = go <$>. parse (p_some p_cmd) <$>. readFile fn
      where
        go (mcmds, ps) = fromMaybe (error (p_error ps)) mcmds

day08 :: io ()
day08
    = readCommands "../inputs/day08.input" >>=. go
      where
        go cmds
            = io_mapM_ putStrLn [part1, part2]
              where
                scr   = processCommands cmds $ initScreen 50 6
                part1 = (++) "part 1: " . showint . pixelsLit $ scr
                part2 = (++) "part 2:\n" . showscreen $ scr
