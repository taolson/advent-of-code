%export day10

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <maybe>
%import <mirandaExtensions>
%import <state>

insn ::= Noop | Addx int
prog   == [insn]

run :: prog -> [int]
run
    = concat . fst . converse (st_mapM step) 1
      where
        step Noop     x = ([x], x)
        step (Addx n) x = ([x, x], x + n)

sample :: [int] -> [int] -> [int]
sample is
    = go is . zip2 [1 ..]
      where
        go [] xs = []
        go is [] = []
        go (i : is) xs
            = [],                            if null xs'
            = snd (hd xs') : go is (tl xs'), otherwise
              where
                xs' = dropWhile ((< i) . fst) xs
        go _ _ = undef || to quiet non-exhaustive warning

display :: [int] -> string
display
    = go 0
      where
        go col [] = []
        go 40  xs = '\n' : go 0 xs
        go col (x : xs)
            = c : go (col + 1) xs
              where
                c = '#', if x - 1 <= col <= x + 1
                  = ' ', otherwise
        go _ _ = undef  || to quiet non-exhaustive warning

readProg :: string -> io prog
readProg fn
    = map (parse . words) <$>. lines <$>. readFile fn
      where
        parse ["noop"]     = Noop
        parse ["addx", ns] = Addx (intval ns)
        parse _            = undef      || added to prevcent compiler from complaining about missing pattern

day10 :: io ()
day10
    = readProg "../inputs/day10.txt" >>=. go
      where
        go prog
            = io_mapM_ putStrLn [part1, part2]
              where
                sig   = run prog
                part1 = (++) "part 1: "  . showint . sum . sample [20, 60 ..] . zipWith (*) [1 ..] $ sig
                part2 = (++) "part 2:\n" . display $ sig
