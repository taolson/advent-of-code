%export day02

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import "intcode"


runNounVerb :: program -> int -> int -> int
runNounVerb program noun verb
    = result
      where
        t             = jitRun (m_insert cmpint 1 noun (m_insert cmpint 2 verb program)) []
        (Just result) = jitGetMem 0 t

findTarget :: program -> int -> (int, int)
findTarget program target
    = (noun, verb)
      where
        base         = runNounVerb program 0 0
        dNoun        = (runNounVerb program 1 0) - base
        dVerb        = (runNounVerb program 0 1) - base
        largest      = max2 cmpint dNoun dVerb
        smallest     = min2 cmpint dNoun dVerb
        q            = (target - base) $div largest
        r            = ((target - base) $mod largest) $div smallest
        (noun, verb) = (q, r), if largest == dNoun
                     = (r, q), otherwise
day02 :: io ()
day02
    = readProgram "../inputs/day02.input" >>=. go
      where
        go program
            = io_mapM_ putStrLn [part1, part2]
              where
                (noun, verb) = findTarget program 19690720
                part1        = (++) "part 1: " . showint $ runNounVerb program 12 2
                part2        = (++) "part 2: " . showint $ 100 * noun + verb
          
