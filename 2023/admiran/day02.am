|| day02.m -- Cube Conundrum


%export day02

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <maybe>
%import <mirandaExtensions>
%import <parser> (>>=)/p_bind (<$>)/p_fmap (<*>)/p_apply (<<)/p_left (>>)/p_right
%import <v3>


rgb  == v3 int
game == (int, [rgb])  || game id, list of cube subsets in R,G,B order

|| find the ids of the games that are possible for the given bag config
possibleIdsFrom :: rgb -> [game] -> [int]
possibleIdsFrom config games
    = map fst . filter possible $ games
      where
        a >=? b          = v3_foldr (&) True $ v3_liftA2 (>=) a b
        possible (_, gc) = all (config >=?) gc

|| find the minimum number of r, g, b cubes that would make the game possible
minCubes :: game -> rgb
minCubes (_, gc) = foldl (v3_max cmpint) (v3_pure 0) gc

rgbPower :: rgb -> int
rgbPower v = v3_foldl (*) 1 v

|| parsing
p_colorCount :: parser rgb
p_colorCount
    = p_liftA2 mkCount (p_spaces >> p_int) (p_spaces >> p_some p_letter)
      where
        mkCount n "red"   = V3 n 0 0
        mkCount n "green" = V3 0 n 0
        mkCount n "blue"  = V3 0 0 n
        mkCount n _       = error "bad color"

p_subset :: parser rgb
p_subset = foldl v3_add (v3_pure 0) <$> (p_spaces >> p_someSepBy (p_char ',') p_colorCount)

p_game :: parser game
p_game = p_liftA2 pair (p_string "Game " >> p_int) (p_char ':' >> p_someSepBy (p_char ';') p_subset) << p_spaces

p_games :: parser [game]
p_games = p_someSepBy p_spaces p_game

readGames :: string -> io [game]
readGames fn
    = doParse <$>. readFile fn
      where
        doParse input
            = fromMaybe err mgs
              where
                (mgs, ps) = parse (p_games << p_end) input
                err       = error (p_error ps)

day02 :: io ()
day02
    = readGames "../inputs/day02.txt" >>=. go
      where
        go games
            = io_mapM_ putStrLn [part1, part2]
              where
                config = V3 12 13 14
                part1  = ("part 1: " ++) . showint . sum . possibleIdsFrom config    $ games
                part2  = ("part 2: " ++) . showint . sum . map (rgbPower . minCubes) $ games

