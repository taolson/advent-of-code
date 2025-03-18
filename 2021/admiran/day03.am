%export day03

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <maybe>
%import <mirandaExtensions>


bit    == num
bitVec == [bit]

bitVecToNum :: bitVec -> num
bitVecToNum
    = foldl addBit 0
      where
        addBit n b = n * 2 + b

mostCommonBit :: bitVec -> bit
mostCommonBit v = 1, if (sum v) * 2 >= #v
                = 0, otherwise

filterVecsBy :: (bit -> bit -> bool) -> [bitVec] -> bitVec
filterVecsBy cmp vs
    = go vs vs
      where
        go vbs []  = error "filterVecsBy: no matches"
        go vbs [v] = v
        go vbs vs
            = go vbs2 vs'
              where
                (bs, vbs1)     = unzip2 (map uncons vbs)
                c              = mostCommonBit bs
                (_, vbs2, vs') = unzip3 (filter match (zip3 bs vbs1 vs))

                match (b, vb, vs) = cmp b c
                uncons (x : xs)   = (x, xs)
                uncons []         = error "uncons []"

readBitVecs :: string -> io [bitVec]
readBitVecs fn
    = map readLine <$>. lines <$>. readFile fn
      where
        readLine   = map bitVal
        bitVal '0' = 0
        bitVal _   = 1  || wildcard to remove non-exhaustive pattern error

day03 :: io ()
day03
    = readBitVecs "../inputs/day03.txt" >>=. go
      where
        go bvecs
            = io_mapM_ putStrLn [part1, part2]
              where
                part1
                    = "part 1: " ++ shownum (gamma * epsilon)
                      where
                        cbits   = map mostCommonBit (transpose bvecs)
                        gamma   = bitVecToNum cbits
                        epsilon = bitVecToNum (map (1 -) cbits)

                part2
                    = "part 2: " ++ shownum (ogr * co2r)
                      where
                        ogr  = (bitVecToNum . filterVecsBy (==)) bvecs
                        co2r = (bitVecToNum . filterVecsBy (~=)) bvecs
