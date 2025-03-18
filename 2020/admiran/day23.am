|| day23.m -- mvector implementation


%export day23

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <maybe>
%import <mirandaExtensions>
%import <vector>
%import <state> (>>=)/st_bind (>>)/st_right

|| shorter synonyms for mvector read/write operations
vrd = v_unsafeRead
vwr = v_unsafeWrite

|| map from a number to the following number
game == mvector int

|| game map, max cup, and current location
gameState == (game, int, int)

doMove :: gameState -> st gameState
doMove (g, maxLbl, x)
    = vrd g x >>= fp1
      where
        fp1 p1    = vrd g p1 >>= fp2 p1
        fp2 p1 p2 = vrd g p2 >>= fp3 p1 p2

        fp3 p1 p2 p3
            = st_bind2 (vrd g p3) (vrd g d) (fnx p1 p3 d)
              where
                d          = (hd . dropWhile inPicked . tl . iterate dest) x
                inPicked n = n == p1 \/ n == p2 \/ n == p3

                dest n
                    = maxLbl, if n' == 0
                    = n',     otherwise
                      where   n' = n - 1

        fnx p1 p3 d nx gd
            = vwr g x  nx     >>
              vwr g p3 gd     >>
              vwr g d  p1     >>
              st_pure (g, maxLbl, nx)

doMoves :: int -> gameState -> gameState
doMoves n gs
    = st_evalState (go n gs) ()
      where
        go n gs
            = st_pure gs,               if n == 0
            = doMove gs >>= go (n - 1), otherwise

makeGame :: string -> gameState
makeGame s
    = st_evalState build ()
      where
        ns   = map digitVal s
        ns'  = tl ns ++ [hd ns]
        g    = v_unsafeThaw (v_rep 10 undef)

        build = st_mapM_ (uncurry (vwr g)) (zip2 ns ns') >>
                st_pure (g, max cmpint ns, hd ns)

makeBigGame :: int -> string -> gameState
makeBigGame sz s
    = st_evalState build ()
      where
        ns1  = map digitVal s
        ns2  = ns1 ++ [max cmpint ns1 + 1 .. sz]
        ns3  = tl ns2 ++ [hd ns2]
        g    = v_unsafeThaw (v_rep (sz + 1) undef)

        build = st_mapM_ (uncurry (vwr g)) (zip2 ns2 ns3) >>
                st_pure (g, sz, hd ns2)

showGameState :: gameState -> string
showGameState (g, _, _)
    = st_execState (vrd g 1 >>= go) []
      where
        go 1 = st_pure ()
        go n = st_modify (++ showint n) >> vrd g n >>= go

starProduct :: gameState -> int
starProduct (g, _, _)
    = st_evalState (vrd g 1 >>= go) ()
      where
        go s1 = st_fmap (* s1) (vrd g s1)

day23 :: io ()
day23
    = io_mapM_ putStrLn [part1, part2]
      where
        input = "487912365"
        part1 = (++) "part 1: " . showGameState . doMoves 100 . makeGame $ input
        part2 = (++) "part 2: " . showint . starProduct . doMoves 10_000_000 . makeBigGame 1_000_000 $ input
