|| day13.m


%export day13

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <maybe>
%import <mirandaExtensions>
%import <parser> (<*)/p_left

depth == int
rng   == int
layer ==(depth, rng)

catches :: int -> layer -> bool
catches time (depth, rng)
    = (depth + time) $mod ((rng - 1) * 2) == 0

severity :: layer -> int
severity (depth, rng) = depth * rng

p_layers :: parser [layer]
p_layers
    = p_many p_layer
      where
        p_layer = p_liftA2 pair (p_int <* p_spaces <* p_char ':' <* p_spaces) p_int <* p_char '\n'

readLayers :: string -> io [layer]
readLayers fn
    = go <$>. parse p_layers <$>. readFile fn
      where
        go (mlayers, ps) = fromMaybe (error (p_error ps)) mlayers

day13 :: io ()
day13
    = readLayers "../inputs/day13.input" >>=. go
      where
        go layers
            = io_mapM_ putStrLn [part1, part2]
              where
                delay = fromJust . find (not . ($any layers) . catches) $ [0 ..]
                part1 = (++) "part 1: " . showint . sum . map severity . filter (catches 0) $ layers
                part2 = (++) "part 2: " . showint $ delay
