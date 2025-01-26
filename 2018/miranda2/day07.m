|| day07.m


%export day07

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <lens>
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import <parser> (<*)/p_left (*>)/p_right

stepDependency == (char, char)
event   == (int, char)
stepMap == m_map char step

(!!!) :: stepMap -> char -> step
m !!! x = fromJust $ m_lookup cmpchar x m


step ::= Step char [char] [char]

stepId       = Lens getf overf where getf (Step a b c) = a; overf fn (Step a b c) = Step (fn a) b c
dependents   = Lens getf overf where getf (Step a b c) = b; overf fn (Step a b c) = Step a (fn b) c
requirements = Lens getf overf where getf (Step a b c) = c; overf fn (Step a b c) = Step a b (fn c)


simState ::= SimState stepMap int [event] string string int

simMap   = Lens getf overf where getf (SimState a b c d e f) = a; overf fn (SimState a b c d e f) = SimState (fn a) b c d e f
idle     = Lens getf overf where getf (SimState a b c d e f) = b; overf fn (SimState a b c d e f) = SimState a (fn b) c d e f
busy     = Lens getf overf where getf (SimState a b c d e f) = c; overf fn (SimState a b c d e f) = SimState a b (fn c) d e f
ready    = Lens getf overf where getf (SimState a b c d e f) = d; overf fn (SimState a b c d e f) = SimState a b c (fn d) e f
complete = Lens getf overf where getf (SimState a b c d e f) = e; overf fn (SimState a b c d e f) = SimState a b c d (fn e) f
time     = Lens getf overf where getf (SimState a b c d e f) = f; overf fn (SimState a b c d e f) = SimState a b c d e (fn f)

makeStepMap :: [stepDependency] -> stepMap
makeStepMap
    = foldl process m_empty
      where
        process m (did, sid)    = addRequirement (getStep sid m) did . addDependent (getStep did m) sid $ m
        addRequirement step rid = m_insert cmpchar (view stepId step) (over requirements (rid :) step)
        addDependent step did   = m_insert cmpchar (view stepId step) (over dependents (did :) step)
        getStep sid             = m_findWithDefault cmpchar (Step sid [] []) sid

findReady :: stepMap -> [char]
findReady = map (view stepId) . filter (null . view requirements) . m_elems

findNewReady :: char -> stepMap -> ([char], stepMap)
findNewReady step stepMap
    = (map (view stepId) ready, stepMap')
      where
        deps     = map (over requirements (delete cmpchar step)) . map (stepMap !!!) . view dependents $ stepMap !!! step
        ready    = filter (null . view requirements) $ deps
        stepMap' = foldl ins stepMap deps
                   where
                     ins m dep = m_insert cmpchar (view stepId dep) dep m

processBusy :: simState -> simState
processBusy state
    = state,  if null . view busy $ state
    = state', otherwise
      where
        sortedBusy        = sortOn cmpint fst $ view busy state
        (time', step)      = hd sortedBusy
        (ready', stepMap') = findNewReady step $ view simMap state

        state' = ( set simMap stepMap'
                 . over idle (+ 1)
                 . set busy (tl sortedBusy)
                 . over ready (ready' ++)
                 . over complete (++ [step])
                 . set time time'
                 ) state
    
processReady :: maybe int -> simState -> simState
processReady baseTime state
    = state'
      where
        readySorted  = sortBy cmpchar . view ready $ state
        numToProcess = view idle state
        busy'        = map adjTime $ take numToProcess readySorted
        ready'       = drop numToProcess readySorted
        adjTime s    = (completionTime baseTime s $ view time state, s)

        state' = ( over idle (subtract (#busy'))
                 . over busy (busy' ++)
                 . set  ready ready'
                 ) state

completionTime :: maybe int -> char -> int -> int
completionTime Nothing  _ time = time + 1
completionTime (Just n) c time = time + n + code c - code 'A' + 1

completionOrder :: maybe int -> simState -> (int, string)
completionOrder baseTime
    = simStep
      where
        simStep (SimState _ _ [] [] c t) = (t, c)
        simStep state = simStep . processReady baseTime . processBusy $ state

|| parsing

p_nl = p_char '\n'

p_stepDependency :: parser stepDependency
p_stepDependency = p_liftA2 pair (p_string "Step" *> p_spaces *> p_letter)
                                 (p_spaces *> p_string "must be finished before step" *> p_spaces *> p_letter)

p_stepDependencies :: parser [stepDependency]
p_stepDependencies = p_some (p_stepDependency <* (p_skipUntil p_any p_nl))

readStepDependencies :: string -> io [stepDependency]
readStepDependencies fn
    = go <$>. parse p_stepDependencies <$>. readFile fn
      where
        go (mdep, ps) = fromMaybe (error (p_error ps)) mdep

day07 :: io ()
day07
    = readStepDependencies "../inputs/day07.input" >>=. go
      where
        go stepDependencies
            = io_mapM_ putStrLn [part1, part2]
              where
                stepMap          = makeStepMap stepDependencies
                initialState     = SimState stepMap 1 [] (findReady stepMap) [] 0
                part1            = (++) "part 1: " . snd . completionOrder Nothing $ initialState
                part2            = (++) "part 2: " . showint . fst . completionOrder (Just 60) . set idle 5 $ initialState
