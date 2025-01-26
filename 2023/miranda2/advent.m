%import <io> (>>.)/io_right
%import <mirandaExtensions>
%import "day01"
%import "day02"
%import "day03"
%import "day04"
%import "day05"
%import "day06"
%import "day07"
%import "day08"
%import "day09"
%import "day10"
%import "day11"
%import "day12"
%import "day13"
%import "day14"
%import "day15"
%import "day16"
%import "day17"
%import "day18"
%import "day19"
%import "day20"
%import "day21"
%import "day22"
%import "day23"
%import "day24"
%import "day25"

progs :: [io ()]
progs
    = [ day01, day02, day03, day04, day05,
        day06, day07, day08, day09, day10,
        day11, day12, day13, day14, day15,
        day16, day17, day18, day19, day20,
        day21, day22, day23, day24, skip        || day25
      ]
      where
        skip = putStrLn "<skipped>"

main :: io ()
main
    = io_mapM_ run $ enumerate progs
      where
        run (i, prog)
            = putStrLn ("--- day " ++ showint (i + 1) ++ " ---") >>. prog >>. putStrLn ""
