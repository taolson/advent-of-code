|| day21.m -- Keypad Conundrum


%export day21

%import "adventLib"
%import <map>
%import <maybe>
%import <state>
%import <v2>


pos    == v2 int
keyMap == m_map char pos

move :: char -> pos -> pos
move '^' (V2 r c) = V2 (r - 1) c
move 'v' (V2 r c) = V2 (r + 1) c
move '<' (V2 r c) = V2 r (c - 1)
move '>' (V2 r c) = V2 r (c + 1)
move _   p        = p

(!!!) :: keyMap -> char -> pos
m !!! c = m_findWithDefault cmpchar (V2 0 0) c m

makeKeyMap :: string -> keyMap
makeKeyMap s = m_fromList cmpchar [(k, V2 r c) | (r, row) <- enumerate (chunk 3 s); (c, k) <- enumerate row]

keyPos, ctrlPos :: keyMap
keyPos  = makeKeyMap "789456123 0A"
ctrlPos = makeKeyMap " ^A<v>"

robotMove == (char, char)
cache     == m_map robotMove int

agent ::= You keyMap | Robot keyMap cache agent

getKeyMap :: agent -> keyMap
getKeyMap (You km)       = km
getKeyMap (Robot km _ _) = km

getKeyPos :: char -> state agent pos
getKeyPos c = st_get >>= getKeyMap .> m_findWithDefault cmpchar (V2 0 0) c .> st_pure

lookCache :: char -> char -> state agent (maybe int)
lookCache a b (You km)        = (Just 1, You km)
lookCache a b (Robot km c ag) = (m_lookup cmprobotMove (a, b) c, Robot km c ag)

updateCache :: char -> char -> int -> state agent int
updateCache a b n (You km) = (n, You km)
updateCache a b n (Robot km c ag)
    = (n, Robot km c' ag)
      where
        c' = m_insert cmprobotMove (a, b) n c

getAgent :: state agent agent
getAgent (Robot km c ag) = (ag, Robot km c ag)
getAgent _  = undef     || added to remove compiler warning

setAgent :: agent -> state agent ()
setAgent ag (Robot km c _) = ((), Robot km c ag)
setAgent ag _ = undef   || added to remove compiler warning

|| check to see if we try to move through the illegal "space" char position
checkLegalFrom :: char -> string -> state agent bool
checkLegalFrom c s
    = st_liftA2 check (getKeyPos ' ') (getKeyPos c)
      where
        check bad start = scanl (converse move) start s |> all (_ne cmppos bad)
                               
|| generate all the possible controller movement commands to move from a to b on the keypad
genCommands :: char -> char -> state agent [string]
genCommands a b
    = st_bind2 (getKeyPos a) (getKeyPos b) mkMoves
      where
        mkMoves (V2 r1 c1) (V2 r2 c2)
            = (mkVert r1 r2 ++ mkHoriz c1 c2) |> permutations |> map (++ "A") |> st_filterM (checkLegalFrom a)

        mkCmds lo hi a b
            = rep n c
              where
                d = b - a
                n = abs d
                c = if' (d < 0) lo hi

        mkVert  = mkCmds '^' 'v'
        mkHoriz = mkCmds '<' '>'

|| find the length of the shortest possible sequence of button presses to move from a to b on this keypad
shortestPath :: char -> char -> state agent int
shortestPath a b
    = lookCache a b >>= check
      where
        check Nothing  = genCommands a b >>= st_mapM nextAgentSequenceCost >>= min cmpint .> updateCache a b
        check (Just n) = st_pure n

|| pass the command sequence to the next agent and get the cost of performing it
nextAgentSequenceCost :: string -> state agent int
nextAgentSequenceCost s
    = getAgent >>= st_runState (sequenceCost s) .> update
      where
        update (n, ag) = st_pure n << setAgent ag 

|| find the length of the shortest possible sequence of button presses to push all buttons in the string s
sequenceCost :: string -> state agent int
sequenceCost s
    = st_foldM tallyShortest (0, 'A') s >>= fst .> st_pure
      where
        tallyShortest (n, c1) c2
            = shortestPath c1 c2 >>= update
              where
                update n' = st_pure (n + n', c2)

complexity :: string -> state agent int
complexity s
    = tally <$> sequenceCost s
      where
        tally cst = cst * numCode
        numCode   = filter digit s |> intval

|| build a stack of robots to control
buildStack :: int -> agent
buildStack n
    = iterate (mkAgent ctrlPos) (You ctrlPos) ! n |> mkAgent keyPos
      where
        mkAgent km agent = Robot km m_empty agent

day21 :: io ()
day21
    = readFile "../inputs/day21.txt" >>= lines .> go
      where
        go codes
            = output [part1, part2]
              where
                part1 = st_evalState (st_mapM complexity codes) (buildStack 2)  |> sum |> showint
                part2 = st_evalState (st_mapM complexity codes) (buildStack 25) |> sum |> showint
