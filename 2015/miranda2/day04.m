|| day04.m


%export day04

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <maybe>
%import <mirandaExtensions>
%import "md5"


findNonce :: (md5State -> bool) -> string -> int
findNonce test key
    = case find (test . digest) [0 ..] of
        (Just n) -> n
        Nothing  -> error "nonce not found"
      where
        digest n = md5Hash $ key ++ showint n

hasZeroDigits :: int -> md5State -> bool
hasZeroDigits n = all (==. '0') . take n . md5Hex

day04 :: io ()
day04
    = io_mapM_ putStrLn [part1, part2]
      where
        key   = "bgvyzdsv"
        part1 = (++) "part 1: " . showint . findNonce (hasZeroDigits 5) $ key
        part2 = (++) "part 2: " . showint . findNonce (hasZeroDigits 6) $ key
