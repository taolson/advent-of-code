|| day02.m


%export day02

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <maybe>
%import <mirandaExtensions>
%import <parser> (<$>)/p_fmap (<*>)/p_apply (<*)/p_left (*>)/p_right

password ::= Password int int char string

valid1 :: password -> bool
valid1 (Password minc maxc req pass)
    = minc <= count <= maxc
      where
        count = length . filter (==. req) $ pass

valid2 :: password -> bool
valid2 (Password p1 p2 req pass)
    = valid p1 $xor valid p2
      where
        len      = #pass
        valid p  = 0 <= p <= len & pass ! (p - 1) ==. req
        a $xor b = (a \/ b) & ~(a & b)

p_password :: parser password
p_password
    = p_liftA4 Password
      (p_posint <* p_char '-')
      (p_posint <* p_spaces)
      (p_any <* p_char ':' <* p_spaces)
      (p_many p_letter <* p_spaces)

readPasswords :: string -> io [password]
readPasswords fn
    = go <$>. readFile fn
      where
        go input
            = fromMaybe err mps
              where
                (mps, ps) = parse (p_many p_password) input
                err       = error (p_error ps)

day02 :: io ()
day02
    = readPasswords "../inputs/day02.txt" >>=. go
      where
        go passwords
            = io_mapM_ putStrLn [part1, part2]
              where
                part1 = (++) "part 1: " . showint . length . filter valid1 $ passwords
                part2 = (++) "part 2: " . showint . length . filter valid2 $ passwords
