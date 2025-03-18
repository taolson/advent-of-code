|| day20.m


%export day20

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <lens>
%import <maybe>
%import <mirandaExtensions>
%import <v3>
%import <parser>  (<*)/p_left (*>)/p_right


|| vector addition operator
(+.) :: v3 int -> v3 int -> v3 int
v +. u = v3_add v u

|| vector * scalar, vector / scalar operators
(*.), (/.) :: v3 int -> int -> v3 int
v *. s = v3_fmap (* s) v
v /. s = v3_fmap ($div s) v

pos == v3 int
vel == v3 int
acc == v3 int

particle ::= Particle int pos vel acc

|| lenses for particle
p_id  = Lens getf overf where getf (Particle a b c d) = a; overf f (Particle a b c d) = Particle (f a) b c d
p_pos = Lens getf overf where getf (Particle a b c d) = b; overf f (Particle a b c d) = Particle a (f b) c d
p_vel = Lens getf overf where getf (Particle a b c d) = c; overf f (Particle a b c d) = Particle a b (f c) d
p_acc = Lens getf overf where getf (Particle a b c d) = d; overf f (Particle a b c d) = Particle a b c (f d)

p_dist :: particle -> int
p_dist p = v3_dist (view p_pos p) (v3_pure 0)

p_tick :: particle -> particle
p_tick (Particle id p v a)
    = Particle id (p +. v') v' a
      where
        v' = v +. a       || include accelerataion component to velocity first

p_atTime :: int -> particle -> particle
p_atTime t (Particle id p v a)
    = Particle id p' v' a
      where
        v' = v +. (a *. t)
        p' = p +. (v *. t) +. ((a *. t2) /. 2)
        t2     = t * (t + 1)

|| parsing

p_getLine :: parser int
p_getLine (il, ic, cs) = (Just ((I# il) - 1), (il, ic, cs))

p_v3 :: parser (v3 int)
p_v3 = mkVec $p_fmap (p_char '<' *> p_someSepBy p_comma p_int <* p_char '>')
       where
         mkVec [x, y, z] = V3 x y z
         mkVec _         = error "parse error on v3"

p_particle :: parser particle
p_particle = p_liftA4 Particle p_getLine (p_string "p=" *> p_v3) (p_string ", v=" *> p_v3) (p_string ", a=" *> p_v3)

|| move the particles, removing any that collide, until all remaining particles are moving away from each other
solve :: [particle] -> [particle]
solve ps
    = ps',       if all snd ds  || all remaining are moving away
    = solve ps', otherwise      || continue to solve with remaining
      where
        ds  = tally . zip2 ps . map p_tick $ ps
        ps' = map fst ds

        || check each particle against the rest for a collision and moving away or not
        tally [] = []
        tally (d : ds)
            = (snd d, away) : tally ds', if survived    || no collision; add the ticked d and its moving-away flag to the survivors
            = tally ds',                 otherwise      || collision; remove d and all it collided with
              where
                pd  = view p_pos $ fst d
                pd' = view p_pos $ snd d

                (ds', away, survived) = foldl collide ([], True, True) ds

                || check for a collision between d and e, and whether they are moving away
                || from each other or not
                collide (ds, away, survived) e
                    = ds' $seq away' $seq survived' $seq (ds', away', survived')        || seq to prevent space leaks
                      where
                        pe        = view p_pos $ fst e
                        pe'       = view p_pos $ snd e
                        away'     = away & v3_dist pd' pe' > v3_dist pd pe
                        nocoll    = _ne cmppos pd pe
                        survived' = survived & nocoll
                        ds'       = e : ds, if nocoll
                                  = ds,     otherwise

readParticles :: string -> io [particle]
readParticles fn
    = go <$>. parse (p_someSepBy (p_char '\n') p_particle) <$>. readFile fn
      where
        go (mparts, ps) = fromMaybe (error (p_error ps)) mparts

day20 :: io ()
day20
    = readParticles "../inputs/day20.input" >>=. go
      where
        go parts
            = io_mapM_ putStrLn [part1, part2]
              where
                part1  = (++) "part 1: " . showint . view p_id . minBy cmpint p_dist . map (p_atTime 1_000_000) $ parts
                part2  = (++) "part 2: " . showint . length . solve $ parts
