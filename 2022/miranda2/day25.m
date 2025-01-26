%export day25

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <maybe>
%import <mirandaExtensions>


snafu == int

snafuVal :: string -> snafu
snafuVal
    = foldl addDigit 0
      where
        addDigit n d = n * 5 + sval d

        sval '=' = -2
        sval '-' = -1
        sval c   = code c - code '0'

showSnafu :: snafu -> string
showSnafu n
    = "0",     if n == 0
    = go [] n, otherwise
      where
        go ds 0 = ds
        go ds n
            = go (d : ds) (n' $div 5)
              where
                s = n $mod 5
                d = sdigit s
                n' = n, if s < 3
                   = n + s, otherwise

        sdigit 3 = '='
        sdigit 4 = '-'
        sdigit n = decode (n + code '0')

day25 :: io ()
day25
    = readFile "../inputs/day25.txt" >>=. go
      where
        go = putStrLn . (++) "part 1: " . showSnafu . sum . map snafuVal . lines
