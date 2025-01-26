|| day16.m


%export day16

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <maybe>
%import <mirandaExtensions> -group
%import <parser> (<$>)/p_fmap (*>)/p_right (<|>)/p_alt


command ::= Spin int | Exchange int int | Partner char char
group   ==  [char]

spin :: group -> int -> group
spin g n
    = back ++ front
      where
        (front, back) = splitAt (#g - n) g

exchange :: group -> int -> int -> group
exchange g i1 i2
    = g,       if i1 == i2
    = go1 0 g, otherwise
      where
        il = min2 cmpint i1 i2
        ih = max2 cmpint i1 i2

        || find first index and capture its value
        go1 i (c : cs)
            = c2 : cs2,           if i == il
            = c : go1 (i + 1) cs, otherwise
              where
                (c2, cs2) = go2 c (i + 1) cs

        go1 _ _ = undef || added to remove compiler warning

        || find second index and replace with first value, returning
        || second value and remaining group
        go2 c1 i (c : cs)
            = (c, c1 : cs),   if i == ih
            = (c2, c : cs2), otherwise
              where
                (c2, cs2) = go2 c1 (i + 1) cs

        go2 _ _ _ = undef || added to remove compiler warning

partner :: group -> char -> char -> group
partner g c1 c2
    = g,     if c1 ==. c2
    = go1 g, otherwise
      where

        || find first match of c1, c2, replace with other, and find the remaining
        go1 (c : cs)
            = (c2 : go2 c2 c1 cs), if c ==. c1
            = (c1 : go2 c1 c2 cs), if c ==. c2
            = c : go1 cs,          otherwise

        go1 _ = undef   || added to remove compiler warning

        || find fnd and replace with rep
        go2 fnd rep (c : cs)
            = rep : cs,           if c ==. fnd
            = c : go2 fnd rep cs, otherwise

        go2 _ _ _ = undef   || added to remove compiler warning


execute :: group -> command -> group
execute g (Spin n)         = spin g n
execute g (Exchange i1 i2) = exchange g i1 i2
execute g (Partner c1 c2)  = partner g c1 c2

executeProgramUntilMatch :: group -> [command] -> [group] -> [group]
executeProgramUntilMatch g commands results
    = results,                                                                    if member cmpgroup results g
    = executeProgramUntilMatch (foldl execute g commands) commands (g : results), otherwise

p_command :: parser command
p_command
    = p_spin <|> p_exchange <|> p_partner
      where
        p_spin     = Spin <$> (p_char 's' *> p_int)
        p_exchange = p_liftA2 Exchange (p_char 'x' *> p_int)    (p_char '/' *> p_int)
        p_partner  = p_liftA2 Partner  (p_char 'p' *> p_letter) (p_char '/' *> p_letter)

p_commands :: parser [command]
p_commands = p_someSepBy (p_char ',') p_command

readCommands :: string -> io [command]
readCommands fn
    = go <$>. parse p_commands <$>. readFile fn
      where
        go (mcmds, ps) = fromMaybe (error (p_error ps)) mcmds

day16 :: io ()
day16
    = readCommands "../inputs/day16.input" >>=. go
      where
        go commands
            = io_mapM_ putStrLn [part1, part2]
              where
                results = reverse $ executeProgramUntilMatch "abcdefghijklmnop" commands []
                part1   = "part 1: " ++ results ! 1
                part2   = "part 2: " ++ results ! (1000000000 $mod #results)
