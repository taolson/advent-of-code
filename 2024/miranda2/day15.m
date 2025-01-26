|| day15.m -- Warehouse Woes


%export day15

%import "adventLib"
%import <map>
%import <maybe>
%import <maybeState>    (>>=?)/mst_bind (<$>?)/mst_fmap (<<?)/mst_left (>>?)/mst_right (<|>?)/mst_alt
%import <v2>            (|+|)/v2_add

pos == v2 int
dir == v2 int

isVert :: dir -> bool
isVert (V2 _ 0) = True
isVert _        = False

toDir :: char -> dir
toDir '^' = V2 (-1) 0
toDir '>' = V2 0 1
toDir 'v' = V2 1 0
toDir '<' = V2 0 (-1)
toDir _   = error "toDir: bad char"


gridElem ::= Empty | Wall | Box | BoxL | BoxR

isDoubleBox :: gridElem -> bool
isDoubleBox BoxL = True
isDoubleBox BoxR = True
isDoubleBox _    = False

|| GPS value calculation
gpsCoord :: pos -> int
gpsCoord (V2 r c) = 100 * r + c

isGpsBox :: gridElem -> bool
isGpsBox Box  = True
isGpsBox BoxL = True               || only the left portion of a box participates in the gps measurement
isGpsBox _    = False


grid     == m_map pos gridElem
gridInfo == (grid, pos)

|| push e in dir d from pos p, returning new pos or Nothing if fail
push :: gridElem -> pos -> dir -> maybeState grid pos
push e p d
    = mst_get >>=? (m_findWithDefault cmppos Empty p' .> go)
      where
        p' = p  |+| d
        pL = p' |+| V2 0 (-1)
        pR = p' |+| V2 0 1

        go Empty = update e p'
        go Wall  = mst_fail
        go b     = pushDouble b,                if isVert d & isDoubleBox b
                 = push b p' d >>? update e p', otherwise

        pushDouble BoxL = push BoxL p' d >>? push BoxR pR d >>? update2 e p' pR         || push BoxL and the associated BoxR on its right
        pushDouble BoxR = push BoxR p' d >>? push BoxL pL d >>? update2 e p' pL         || push BoxR and the associated BoxL on its left
        pushDouble _    = undef || added to remove compiler warning

        update Empty p = mst_pure p <<? mst_modify (m_delete cmppos p)
        update e     p = mst_pure p <<? mst_modify (m_insert cmppos p e)

        update2 Empty p1 p2 = mst_pure p1 <<? mst_modify (m_delete cmppos p1 . m_delete cmppos p2)
        update2 BoxL  p1 p2 = mst_pure p1 <<? mst_modify (m_insert cmppos p1 BoxL . m_delete cmppos p2)
        update2 BoxR  p1 p2 = mst_pure p1 <<? mst_modify (m_insert cmppos p1 BoxR . m_delete cmppos p2)
        update2 _     _  _  = undef     || added to remove compiler warning

|| robot push from pos in direction dir or return same p and state if fail
move :: pos -> dir -> maybeState grid pos
move p d = push Empty p d <|>? mst_pure p

execRobot :: [dir] -> gridInfo -> grid
execRobot ds (g, p) = mst_execState (mst_foldM move p ds) g

makeGridInfo :: [string] -> gridInfo
makeGridInfo rows
    = foldl insGrid (m_empty, v2_pure 0) xs
      where
        xs = [(V2 r c, e) | (r, row) <- enumerate rows; (c, e) <- enumerate row; e ~=. '.']
        
        insGrid (g, r) (p, c)
            = go c
              where
                ins e = (m_insert cmppos p e g, r)

                go '#' = ins Wall
                go 'O' = ins Box
                go '@' = (g, p)
                go c   = error ("makeGrid: bad char " ++ showchar c)

|| expand grid by doubling components horizontally
expandGridInfo :: gridInfo -> gridInfo
expandGridInfo (g, (V2 r c))
    = (g', V2 r (c * 2))
      where
        g' = m_toList g |> concatMap expand |> m_fromList cmppos

        expandElem Box = (BoxL, BoxR)
        expandElem e   = (e, e)

        expand (V2 r c, e)
            = [(V2 r (c * 2),el), (V2 r (c * 2 + 1), er)]
              where
                (el, er) = expandElem e
            
makeMoves :: [string] -> [dir]
makeMoves = concat .> map toDir

parseInput :: string -> (gridInfo, [dir])
parseInput
     = lines .> splitWhen null .> go
       where
         go [a, b] = (makeGridInfo a, makeMoves b)
         go _      = error "parse error"

day15 :: io ()
day15
    = readFile "../inputs/day15.txt" >>= parseInput .> go
      where
        go (gi, moves)
            = output [part1, part2]
              where
                process = execRobot moves .> m_toList .> filter (snd .> isGpsBox) .> map (fst .> gpsCoord) .> sum .> showint
                part1   = process gi
                part2   = expandGridInfo gi |> process
