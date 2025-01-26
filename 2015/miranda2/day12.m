|| day12.m


%export day12

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <maybe>
%import <mirandaExtensions>
%import <parser> (<$>)/p_fmap (<*)/p_left (*>)/p_right (<|>)/p_alt


keyVal == (string, json)

json   ::= Num int | Str string | Arr [json] | Obj bool [keyVal]


p_quote :: parser char
p_quote = p_char '\"'

p_stringLit :: parser string
p_stringLit = p_quote *> p_word <* p_quote

p_num :: parser json
p_num = Num <$> p_int

p_jstr :: parser json
p_jstr = Str <$> p_stringLit

p_array :: parser json
p_array = Arr <$> (p_char '[' *> p_manySepBy p_comma p_json <* p_char ']')

p_keyVal :: parser keyVal
p_keyVal = p_liftA2 pair (p_stringLit <* p_char ':') p_json

p_obj :: parser json
p_obj = mkObj <$> (p_char '{' *> p_manySepBy p_comma p_keyVal <* p_char '}')
        where
          mkObj kvs = Obj (any (isRedStr . snd) kvs) kvs        || mark objects with any "red" values

          isRedStr (Str "red") = True
          isRedStr _           = False

p_json :: parser json
p_json = p_num <|> p_jstr <|> p_array <|> p_obj

tally :: bool -> json -> int
tally noReds js
    = go js
      where
        go (Num n)  = n
        go (Str _)  = 0
        go (Arr xs) = sum . map go $ xs
        go (Obj hasRed xs)
            = 0,                         if noReds & hasRed
            = sum . map (go . snd) $ xs, otherwise

readJson :: string -> io json
readJson fn
    = go <$>. parse p_json <$>. readFile fn
      where
        go (mjson, ps) = fromMaybe (error (p_error ps)) mjson

day12 :: io ()
day12
    = readJson "../inputs/day12.input" >>=. go
      where
        go json
            = io_mapM_ putStrLn [part1, part2]
              where
                  part1 = (++) "part 1: " . showint . tally False $ json
                  part2 = (++) "part 2: " . showint . tally True  $ json
