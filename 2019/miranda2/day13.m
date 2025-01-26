%export day13

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import "intcode"


point == (int, int)

tile ::= Empty | Wall | Block | Paddle | Ball

toTile :: num -> tile
toTile 0 = Empty
toTile 1 = Wall
toTile 2 = Block
toTile 3 = Paddle
toTile 4 = Ball
toTile _ = error "toTile: bad input"

screenState ::= ScreenState
                (m_map point tile)  || screen
                num                 || score
                point               || ball
                point               || paddle

readScreenState :: jitState -> (screenState, jitState)
readScreenState
    = addTiles (ScreenState m_empty 0 (0, 0) (0, 0))
      where
        addTiles ss t
            = (ss, t),          if null out
            = addTiles ss' t3,  if x == -1 & y == 0
            = addTiles ss'' t3, otherwise
              where
                ScreenState screen score ball paddle = ss
                (out, _)  = jitGetAllOutput t
                (x, t1)   = jitGetOutput t
                (y, t2)   = jitGetOutput t1
                (n, t3)   = jitGetOutput t2
                score'    = score + n
                ss'       = ScreenState screen score' ball paddle
                loc       = (x, y)
                tile      = toTile n
                screen'   = m_insert cmppoint loc tile screen
                ball'     = loc,    if _eq cmptile tile Ball
                          = ball,   otherwise
                paddle'   = loc,    if _eq cmptile tile Paddle
                          = paddle, otherwise
                ss''      = ScreenState screen' score ball' paddle'

runGame :: jitState -> screenState
runGame t
    = ss,                       if isHalt rs
    = runGame (jitContinue t2), otherwise
      where
        (ss, t1) = readScreenState t
        rs       = jitGetRunState t1
        ScreenState _ _ (bx, _) (px, _) = ss
        js       = signum (bx - px)
        t2       = jitPutInput t1 js
        
day13 :: io ()
day13
    = readProgram "../inputs/day13.input" >>=. (go . jitReset)
      where
        go t
            = io_mapM_ putStrLn [part1, part2]
              where
                (ScreenState screen1 _ _ _, _) = readScreenState (jitContinue t)
                ScreenState  _ score2 _ _ = runGame (jitContinue (jitSetMem 0 2 t))
                part1  = (++) "part 1: " . showint . length . filter (_eq cmptile Block) . m_elems $ screen1
                part2  = (++) "part 2: " . showint $ score2
