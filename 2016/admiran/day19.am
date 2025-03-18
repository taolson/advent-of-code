|| day19.m


%export day19

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <dequeue>
%import <maybe>
%import <mirandaExtensions>


makeElfQueue :: int -> int -> dequeue int
makeElfQueue start stop = dq_fromList [start .. stop]

play1 :: dequeue int -> int
play1 q
    = case fromJust $ dq_viewL q of (e1, q1) ->
        case dq_null q1 of
            False -> case fromJust $ dq_viewL q1 of (_, q2) ->
                       case dq_addR e1 q2 of q3 -> play1 q3
            True  -> e1
                     
play2 :: dequeue int -> dequeue int -> int -> int
play2 qa qb count
    = case fromJust $ dq_viewL qa of (ea, qa1) ->
        case count < 3 of
          False -> case count - 1 of count' ->
                     case fromJust $ dq_viewL qb of (_, qb1) ->
                       case even count of
                         False -> case fromJust $ dq_viewL qb1 of (eb, qb2) ->
                                    case dq_addR eb qa1 of qa' ->
                                      case dq_addR ea qb2 of qb' ->
                                        play2 qa' qb' count'
                         True  -> case dq_addR ea qb1 of qb' ->
                                    play2 qa1 qb' count'
          True  -> ea

day19 :: io ()
day19
    = io_mapM_ putStrLn [part1, part2]
      where
        elfCount = 3012210
        half     = elfCount $div 2
        q1       = makeElfQueue 1 elfCount
        qa       = makeElfQueue 1 half
        qb       = makeElfQueue (half + 1) elfCount
        part1    = (++) "part 1: " . showint $ play1 q1
        part2    = (++) "part 2: " . showint $ play2 qa qb elfCount
