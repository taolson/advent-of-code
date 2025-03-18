|| initAdventFiles.m -- initialize all of the advent files to blank templates


%import <io>                    (>>=)/io_bind
%import <maybe>
%import <mirandaExtensions>

fileDigits :: int -> string
fileDigits n
    = '0' : s, if #s == 1
    = s,       otherwise
      where
        s = showint n

replace :: string -> string -> string -> string
replace s1 s2
    = go
      where
        len   = #s1
        go [] = []
        go s
            = s2 ++ go sb,      if sa ==$ s1
            = hd s : go (tl s), otherwise
              where
                (sa, sb) = splitAt len s

initFile :: string -> int -> io ()
initFile template n
    = writeFile fn $ replace "<DAY>" mn template
      where
        mn = "day" ++ fileDigits n
        fn = mn ++ ".m"

main :: io ()
main
    = readFile "template.m" >>= go
      where
        go template = io_mapM_ (initFile template) [1 .. 25]
