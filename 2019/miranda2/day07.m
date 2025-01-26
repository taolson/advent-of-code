%export day07

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import "intcode"


runSequence :: program -> num -> [num] -> num
runSequence prog = foldl process
                   where
                     process sig x = (fst . jitGetOutput . jitRun prog) [x, sig]

runFeedbackSequence :: program -> num -> [num] -> num
runFeedbackSequence prog sig seq
    = go sig (map initTask seq) []
      where
        initTask x       = ((converse jitPutInput) x . jitReset) prog
        go sig [] halted = sig
        go sig (t : ts) halted
            = go sig' ts            (t'' : halted), if isHalt (jitGetRunState t'')
            = go sig' (ts ++ [t'']) halted,         otherwise
              where
                t'          = jitContinue (jitPutInput t sig)
                (sig', t'') = jitGetOutput t'

day07 :: io ()
day07
    = readProgram "../inputs/day07.input" >>=. go
      where
        go prog
            = io_mapM_ putStrLn [part1, part2]
              where
                best1 = max cmpint . map (runSequence prog 0)         $ permutations [0 .. 4]
                best2 = max cmpint . map (runFeedbackSequence prog 0) $ permutations [5 .. 9]
                part1 = "part 1: " ++ showint best1
                part2 = "part 2: " ++ showint best2
