|| day25.m


%export day25

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <mirandaExtensions>


|| quick modular exponentiation using squaring
expMod :: int -> int -> int -> int
expMod m a b
    = go 1 a b
      where
        go r _ 0 = r
        go r a b
            = go r' a' b'
              where
                b' = b .>>. 1
                a' = a * a $mod m
                r' = r * a $mod m, if b .&. 1 == 1
                   = r,            otherwise

|| discrete logarithm search by successive modular multiplication
logMod :: int -> int -> int -> int
logMod m a b
    = go 0 1
      where
        go n r
            = n,                                if r == b
            = n $seq go (n + 1) (r * a $mod m), otherwise

day25 :: io ()
day25
    = putStrLn $ "part 1: " ++ showint (expMod m k2 l1)
      where
        p  = 7
        m  = 20201227
        k1 = 14205034
        k2 = 18047856
        l1 = logMod m p k1
