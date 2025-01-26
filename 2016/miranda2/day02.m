|| day02.m


%export day02

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <map>
%import <maybe>
%import <mirandaExtensions>


direction ::= U | D | L | R

toDirection :: char -> direction
toDirection 'U' = U
toDirection 'D' = D
toDirection 'L' = L
toDirection 'R' = R
toDirection _   = error "toDirection: bad char"

sequence == [direction]

toSequence :: string -> sequence
toSequence = map toDirection

key == char

keypadMove == m_map direction key
keypad     == m_map  key keypadMove

updateKeypad :: key -> direction -> key -> keypad -> keypad
updateKeypad key dir val
    = id,                                                  if key ==. ' ' \/ val ==. ' '
    = m_adjust cmpkey (m_insert cmpdirection dir val) key, otherwise

addKey :: (keypad, key) -> (key, key) -> (keypad, key)
addKey (pad, leftKey) (key, upKey)
    = (updatePad pad, key)
      where
        updatePad
            = id,                                                 if key ==. ' '
            = updateU . updateD . updateL . updateR . addDefault, otherwise

        addDefault = m_insert cmpkey key $ m_fromList cmpdirection . zip2 [U, D, L, R] $ repeat key
        updateU    = updateKeypad key     U upKey
        updateD    = updateKeypad upKey   D key
        updateL    = updateKeypad key     L leftKey
        updateR    = updateKeypad leftKey R key

keypadRow :: (keypad, string) -> string -> (keypad, string)
keypadRow (pad, upRow) row
    = (updatePad, row)
      where
        updatePad = fst . foldl addKey (pad, ' ') $ zip2 row upRow

keypadFromStrings :: [string] -> keypad
keypadFromStrings = fst . foldl keypadRow (m_empty, repeat ' ')

squareKeypad
    = keypadFromStrings
      [ " 123 "
      , " 456 "
      , " 789 "
      ]

diamondKeypad
    = keypadFromStrings
      [ "   1   "
      , "  234  "
      , " 56789 "
      , "  ABC  "
      , "   D   "
      ]

codeForSequences :: keypad -> [sequence] -> string
codeForSequences pad
    = fst . foldl codeForSequence ("", '5')
      where
        codeForSequence (code, key) [] = (code ++ [key] , key)
        codeForSequence (code, key) (dir:dirs)
            = codeForSequence (code, move) dirs
              where
                kpm  = fromJust . m_lookup cmpchar       key $ pad
                move = fromJust . m_lookup cmpdirection  dir $ kpm

(.=<<) = converse (>>=.)

day02 :: io ()
day02
||    = go .=<< map toSequence <$>. lines <$>. readFile "../inputs/day02.input"
    = readFile "../inputs/day02.input" >>=. (go . map toSequence . lines)
      where
        go seqs
            = io_mapM_ putStrLn [part1, part2]
              where
                part1 = (++) "part 1: " . codeForSequences squareKeypad  $ seqs
                part2 = (++) "part 2: " . codeForSequences diamondKeypad $ seqs
