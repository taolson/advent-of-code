|| day15.m


%export day15

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <lens>
%import <mirandaExtensions>
%import <vector>
%import <state> (>>=)/st_bind (<$>)/st_fmap (<<)/st_left


game ::= Game int (mvector int) int int

g_rounds = Lens getf overf where getf (Game r s a l) = r; overf fn (Game r s a l) = Game (fn r) s a l
g_spoken = Lens getf overf where getf (Game r s a l) = s; overf fn (Game r s a l) = Game r (fn s) a l
g_age    = Lens getf overf where getf (Game r s a l) = a; overf fn (Game r s a l) = Game r s (fn a) l
g_last   = Lens getf overf where getf (Game r s a l) = l; overf fn (Game r s a l) = Game r s a (fn l)

emptyGame :: int -> game
emptyGame n
    = Game n spoken 0 0
      where
        spoken = v_unsafeThaw $ v_rep n 0

stepGame :: game -> st game
stepGame (Game rounds spoken age last)
    = v_unsafeRead spoken last >>= go
      where
        go lookup
            = age' $seq last' $seq st_pure (Game rounds spoken age' last') << v_unsafeWrite spoken last age
              where
                age'  = age + 1
                last' = if' (lookup == 0) 0 (age - lookup)

initGame :: game -> int -> st game
initGame game n = set g_last n <$> stepGame game

makeGame :: int -> [int] -> st game
makeGame n = st_foldM initGame (emptyGame n)
    
playGame :: game -> st game
playGame game
    = st_pure game,               if view g_age game >= view g_rounds game
    = stepGame game >>= playGame, otherwise

day15 :: io ()
day15
    = io_mapM_ putStrLn [part1, part2]
      where
        input = [18,8,0,5,4,1,20]
        game1 = st_evalState (makeGame 2020       input >>= playGame) ()
        game2 = st_evalState (makeGame 30_000_000 input >>= playGame) ()
        part1 = (++) "part 1: " . showint . view g_last $ game1
        part2 = (++) "part 2: " . showint . view g_last $ game2
