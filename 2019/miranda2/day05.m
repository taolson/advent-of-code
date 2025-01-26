%export day05

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <avl>
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import "intcode"


process :: program -> int -> [int]
process prog inp
    = result
      where
        t            = jitRun prog [inp]
        (result, _)  = jitGetAllOutput t

printResults :: int -> [(int, int)] -> string
printResults part []            = error "printResults []"
printResults part [(r, i)]      = "part " ++ showint part ++ ": " ++ showint r
printResults part ((r, i) : rs) = printResults part rs,                                                                      if r == 0
                                = "diagnostic " ++ showint i ++ " failed with code " ++ showint r ++ "\n" ++ printResults part rs, otherwise

day05 :: io ()
day05
    = readProgram "../inputs/day05.input" >>=. go
      where
        go prog
            = io_mapM_ putStrLn [part1, part2]
              where
                out1  = process prog 1
                out2  = process prog 5
                part1 = printResults 1 (zip2 out1 [1 ..])
                part2 = printResults 2 (zip2 out2 [1 ..])
