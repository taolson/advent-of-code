|| day08.m


%export day08

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <mirandaExtensions>


hexval :: char -> int
hexval c
    = code c - code '0',        if '0' <=. c <=. '9'
    = code c - code 'a' + 10,   if 'a' <=. c <=. 'f'
    = error "hexval: bad char", otherwise

decodeString :: string -> string
decodeString ('\"' : cs)
    = go cs
      where
        escape 'x' (a : b : cs) = decode (hexval a * 16 + hexval b) : go cs
        escape c cs = c : go cs

        go ['\"']          = []
        go ('\\' : c : cs) = escape c cs
        go (c : cs )       = c : go cs
        go _               = error "decodeString: bad string"

decodeString _ = error "decodeString: not enclosed in quotes"

encodeString :: string -> string
encodeString
    = ('\"' :) . go
      where
        go [] = ['\"']
        go ('\"' : cs) = '\\' : '\"' : go cs
        go ('\\' : cs) = '\\' : '\\' : go cs
        go (c : cs)    = c : go cs

lenDiff :: (string -> string) -> string -> int
lenDiff f s = abs (#s - #f s)

day08 :: io ()
day08
    = readFile "../inputs/day08.input" >>=. (go . lines)
      where
        go input
            = io_mapM_ putStrLn [part1, part2]
              where
              part1 = (++) "part 1: " . showint . sum . map (lenDiff decodeString) $ input
              part2 = (++) "part 2: " . showint . sum . map (lenDiff encodeString) $ input

