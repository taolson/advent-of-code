|| day23.m


%export day23

%import <map>
%import <maybe>
%import <mirandaExtensions>

(!!!) :: m_map int int -> int -> int
m !!! i = fromJust (m_lookup cmpint i m)

|| map from a number to the following number
game == m_map int int

|| game map, max cup, and current location
gameState == (game, int, int)

|| entirely sequential implementation to prevent space leaks
move :: gameState -> gameState
move (g, maxLbl, x)
    = case g !!! x of
        p1 -> case g !!! p1 of
                p2 -> case g !!! p2 of
                        p3 -> case g !!! p3 of
                               nx -> case m_insert cmpint x nx g of
                                       g1 -> case (hd . dropWhile inPicked . tl . iterate dest) x of
                                               d -> case m_insert cmpint p3 (g !!! d) g1 of
                                                      g2 -> case m_insert cmpint d p1 g2 of
                                                              g3 -> (g3, maxLbl, nx)
                              where
                                inPicked n = n == p1 \/ n == p2 \/ n == p3
                                dest     n = maxLbl, if n' == 0
                                           = n',     otherwise
                                             where n' = n - 1

readGame :: string -> gameState
readGame [] = error "readGame []"
readGame (fc : cs)
    = go m_empty fn cs
      where
        toint c          = intval [c]
        fn               = toint fc
        go g pn []       = (m_insert cmpint pn fn g, fst (m_last g), fn)
        go g pn (c : cs) = go (m_insert cmpint pn cn g) cn cs where cn = toint c

makeBigGame :: int -> gameState -> gameState
makeBigGame sz (g, ml, st)
    = case foldl addCup (g, keyForVal st g) [ml + 1 .. sz] of
        (g', _) -> (m_insert cmpint sz st g', sz, st)
      where
        addCup (g, n) m = case m_insert cmpint n m g of g' -> (g', m)

        keyForVal x m
            = go $ m_toList g
              where
                go [] = -1
                go ((k, v) : as)
                    = k,     if v == x
                    = go as, otherwise

showGameState :: gameState -> string
showGameState (g, _, _)
    = go (g !!! 1)
      where
        go 1 = []
        go n = showint n ++ go (g !!! n)

starProduct :: gameState -> int
starProduct (g, _, _)
    = s1 * s2
      where
        s1 = g !!! 1
        s2 = g !!! s1

getCup :: int -> gameState -> int
getCup n (g, _, _)
    = g !!! n

doPart1 :: gameState -> gameState
doPart1
    = go 100
      where
        go 0 gs = gs
        go n gs = case move gs of gs' -> go (n - 1) gs'

doPart2 :: gameState -> gameState
doPart2 gs
    = case makeBigGame 1000000 gs of gs2 -> go 10000000 gs2
      where
        go 0 gs = gs
        go n gs = case move gs of gs' -> go (n - 1) gs'

day23 :: string
day23
    = lay [part1, part2]
      where
        game  = readGame "487912365"
        part1 = (++) "part 1: " . showGameState . doPart1 $ game
        part2 = (++) "part 2: " . showint . starProduct . doPart2 $ game
