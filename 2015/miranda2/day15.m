|| day15.m


%export day15

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <mirandaExtensions>


constraint == int -> int -> int -> bool

noConstraint :: constraint
noConstraint _ _ _ = True

calorieConstraint :: constraint
calorieConstraint a b c
    = 5 * a + b + 6 * c + 8 * d == 500
      where
        d = 100 - a - b - c

|| score a cookie made from the tsp amounts of the ingredients
cookieScore :: int -> int -> int -> int
cookieScore a b c
    = 0,                     if cap < 0 \/ dur < 0
    = cap * dur * fla * tex, otherwise
      where
        d   = 100 - a - b - c
        cap = 5 * a - b - d
        dur = 3 * b - a - c
        fla = 4 * c
        tex = 2 * d

|| create a list of recipes, given a constraint on its ingredients
recipes :: constraint -> [int]
recipes constrain
    = [cookieScore a b c | a <- [1 .. 100]; b <- [1 .. 100 - a]; c <- [1 .. 100 - b]; constrain a b c]

day15 :: io ()
day15
    = io_mapM_ putStrLn  [part1, part2]
      where
        process = showint . max cmpint . recipes
        part1   = (++) "part 1: " $ process noConstraint
        part2   = (++) "part 2: " $ process calorieConstraint
