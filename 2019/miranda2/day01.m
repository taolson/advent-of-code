%import <mirandaExtensions>

%export day01

%import <io> (>>=.)/io_bind (<$>.)/io_fmap


computeFuel :: (int -> int) -> [int] -> int
computeFuel fn ms = sum (map fn ms)

fuelForModule :: int -> int
fuelForModule m = m $div 3 - 2

fuelForTotalMass :: int -> int
fuelForTotalMass m
    = sum (takeWhile (> 0) masses) 
      where
        masses = tl (iterate fuelForModule m)

day01 :: io ()
day01
    = readFile "../inputs/day01.input" >>=. go
      where
        go input
            = io_mapM_ putStrLn [part1, part2]
              where
	        modules = map intval . lines $ input
	        fuel1   = computeFuel fuelForModule modules
	        fuel2   = computeFuel fuelForTotalMass modules
                part1   = "part 1: " ++ showint fuel1
                part2   = "part 2: " ++ showint fuel2
