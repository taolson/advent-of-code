|| day22.m


%export day22

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <dequeue>
%import <maybe>
%import <mirandaExtensions>
%import <set>

dq_take :: int -> dequeue * -> dequeue *
dq_take
    = go dq_empty
      where
        go r n q
            = r,                if n <= 0
            = go r' (n - 1) q', otherwise
              where
                (x, q') = fromJust $ dq_viewL q
                r'      = dq_addR x r

deck == dequeue int

cmpdeck :: deck -> deck -> ordering
cmpdeck d1 d2 = cmplist cmpint (dq_toList d1) (dq_toList d2)
    
readDeck :: [string] -> (deck, [string])
readDeck
    = go dq_empty
      where
        go deck []      = (deck, [])
        go deck (s : ss)
            = (deck, ss),                         if null s
            = go (dq_addR (intval s) deck) ss, otherwise

readDecks :: [string] -> [deck]
readDecks
    = go []
      where
        go decks [] = decks
        go decks lines
            = go (decks ++ [deck]) rest
              where
                (deck, rest) = readDeck (tl lines)

doRound :: (deck, deck) -> (deck, deck)
doRound (d1, d2)
    = (d1b, d2b)
      where
        (t1, d1a) = fromJust $ dq_viewL d1
        (t2, d2a) = fromJust $ dq_viewL d2
        win1      = t1 > t2
        d1b       = dq_addR t2 (dq_addR t1 d1a), if win1
                  = d1a,                            otherwise
        d2b       = d2a,                            if win1
                  = dq_addR t1 (dq_addR t2 d2a), otherwise

play :: (deck, deck) -> deck
play (d1, d2)
    = d1,                        if dq_null d2
    = d2,                        if dq_null d1
    = play . doRound $ (d1, d2), otherwise

deckPair == (deck, deck)
seenSet  == s_set deckPair

doRoundRecursive :: deck -> deck -> (deck, deck)
doRoundRecursive d1 d2
    = (d1b, d2b)
      where
        (t1, d1a) = fromJust $ dq_viewL d1
        (t2, d2a) = fromJust $ dq_viewL d2
        recD1     = dq_size d1a >= t1
        recD2     = dq_size d2a >= t2
        d1b       = dq_addR t2 (dq_addR t1 d1a), if win1
                  = d1a,                         otherwise
        d2b       = d2a,                         if win1
                  = dq_addR t1 (dq_addR t2 d2a), otherwise

        win1
            = win',    if recD1 & recD2
            = t1 > t2, otherwise
              where
                (win', _, _) = playRecursive (dq_take t1 d1a) (dq_take t2 d2a)
    
playRecursive :: deck -> deck -> (bool, deck, deck)
playRecursive
    = go s_empty
      where
        go seen d1 d2
            = (True,  d1, d2),  if s_member cmpdeckPair (d1, d2) seen
            = (True,  d1, d2),  if dq_null d2
            = (False, d1, d2),  if dq_null d1
            = go seen' d1' d2', otherwise
              where
                seen'      = s_insert cmpdeckPair (d1, d2) seen
                (d1', d2') = doRoundRecursive d1 d2

computeScore :: deck -> int
computeScore = sum . zipWith (*) [1 .. ] . reverse . dq_toList

day22 :: io ()
day22
    = readFile "../inputs/day22.txt" >>=. (go . readDecks . lines)
      where
        go decks
            = io_mapM_ putStrLn [part1, part2]
              where
                d1              = decks ! 0
                d2              = decks ! 1
                score1          = computeScore . play $ (d1, d2)
                (win, d1', d2') = playRecursive d1 d2
                score2          = computeScore (if' win d1' d2')
                part1           = "part 1: " ++ showint score1
                part2           = "part 2: " ++ showint score2
