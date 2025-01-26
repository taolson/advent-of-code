|| day04.m


%export day04

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <bag>
%import <maybe>
%import <mirandaExtensions>
%import <parser> (<<)/p_left (>>)/p_right (<|>)/p_alt


room ::= Room string int string string string bool      || name, ident, checksum, computed, decoded, valid

getIdent :: room -> int
getIdent (Room _ i _ _ _ _) = i

getDecoded :: room -> string
getDecoded (Room _ _ _ _ d _) = d

getValid :: room -> bool
getValid (Room _ _ _ _ _ v) = v

computeChecksum :: string -> string
computeChecksum
    = map fst . sortBy (descending cmpint snd) . charCounts
      where
        charCounts = b_toList . b_fromList cmpchar . filter letter

validChecksum :: string -> string -> bool
validChecksum cs ccs
    = #cs == 5 & #ccs >= 5 & take 5 ccs ==$ cs

decodeName :: string -> int -> string
decodeName name ident
    = map decodeChar name
      where
        codeA          = code 'a'
        decodeChar '-' = ' '
        decodeChar c   = decode . (codeA +) $ (ident + code c - codeA) $mod 26

p_room :: parser room
p_room
    = p_liftA3 mkRoom p_roomName p_int (p_inBrackets p_word) << p_spaces
      where
        p_inBrackets p = p_char '[' >> p << p_char ']'
        p_roomName     = p_some (p_letter <|> p_char '-')

        mkRoom name ident cs
            = Room name ident cs ccs dec v
              where
                ccs = computeChecksum name
                dec = decodeName name ident
                v   = validChecksum cs ccs
          
isInfixOf :: ordI * -> [*] -> [*] -> bool
isInfixOf cmp ts = any (isPrefixOf cmp ts) . tails

findNorthPole :: [room] -> room
findNorthPole = fromMaybe (error "not found!") . find (isInfixOf cmpchar "northpole" . getDecoded)

readRooms :: string -> io [room]
readRooms fn
    = go <$>. parse (p_many p_room) <$>. readFile fn
      where
        go (mrooms, ps) = fromMaybe (error (p_error ps)) mrooms

day04 :: io ()
day04
    = readRooms "../inputs/day04.input" >>=. go
      where
        go rooms
            = io_mapM_ putStrLn [part1, part2]
              where
                np    = findNorthPole rooms
                part1 = (++) "part 1: " . showint . sum . map getIdent . filter getValid $ rooms
                part2 = "part 2: " ++ "id: " ++ showint (getIdent np)  ++ " decrypted: " ++ getDecoded np
