|| day20.m -- Pulse Propagation
||
|| Note: this solution turns out to be overkill: I was assuming that we had to find a periodic count to the state in part 2,
|| so to track it easily I converted the state held in each module to an index into an mvector that was updated
|| It probably should be rewritten now that I know what is required in part 2, to simplify all the parsing and remove the
|| conversion / generation of mids / pids.


%export day20

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <dequeue>
%import <lens>
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import <state> (>>=)/st_bind (<$>)/st_fmap (<<)/st_left (>>)/st_right
%import <vector>


pulse ::= Low | High

isLow, isHigh :: pulse -> bool
isLow Low   = True
isLow High  = False

isHigh Low  = False
isHigh High = True

p_not :: pulse -> pulse
p_not Low  = High
p_not High = Low


modName   == string
modId     == int
pulseStId == int                || index into pulseSt
signal    == (int, pulse)       || id for Conjunction to use to index into its saved pulse st, pulse value


|| different modules and the state they hold
modType ::=
    Broadcaster                 |
    FlipFlop    pulseStId       |       || flip flop state
    Conjunction pulseStId int           || base index and size for remembered pulse states

module == (modName, modId, modType, [(modId, int)])    || name, id, type, and list of destination modId, idx pairs

|| lenses for module
mod_name  = lensTup4_0
mod_id    = lensTup4_1
mod_type  = lensTup4_2
mod_dests = lensTup4_3

net == m_map modName module


|| parsing and building the net
|| idea is to create a vector of modules, with modNames replaced with modIds which are indices into the vector
|| state for each module is kept in a mutable vector of pulse values, with each module pointing to a single value
|| or contiguous range of values in the pulseSt vector

parseSt == (m_map modName modId, m_map modName [modName], int)

|| lookup or generate a unique modId for a name 
genId :: modName -> state parseSt modId
genId n st
   = (gen, st'),         if isNothing mid
   = (fromJust mid, st), otherwise
     where
       (modm, srcm, gen) = st

       mid   = m_lookup cmpmodName n modm
       modm' = m_insert cmpmodName n gen modm
       gen'  = gen + 1
       st'   = (modm', srcm, gen')

allocPulseStId :: int -> state parseSt int
allocPulseStId n (modm, srcm, gen)
    = (gen, (modm, srcm, gen + n))

|| add a source to a list of source names for a module name
addSrc :: modName -> modName -> state parseSt ()
addSrc sn dn (modm, srcm, gen)
    = ((), (modm, srcm', gen))
      where
        srcm' = m_insertWith cmpmodName (++) dn [sn] srcm

|| partially parse a mod specification, collecting unique ids for its names
|| and collecting a list of sources for Conjunctions
parseMod :: string -> state parseSt (modName, char, modId, [modName])
parseMod s
    = chooseType . splitOneOf cmpchar "-> ," $ s
      where
        chooseType ((c : mn) : dests)
            = go (c : mn) c dests, if c ==. 'b'      || broadcaster
            = go mn c dests,       otherwise         || other modules
        
        chooseType xs = error "parseMod: bad parse"

        go mn c dests
            = mkInfo <$> genId mn << st_mapM (addSrc mn) dests
              where
                mkInfo mid = (mn, c, mid, dests)

getSrcs :: modName -> state parseSt [modName]
getSrcs n (modm, srcm, gen)
    = (fromMaybe [] (m_lookup cmpmodName n srcm), (modm, srcm, gen))

mkDstId :: modName -> modName -> state parseSt (modId, int)
mkDstId n d (modm, srcm, gen)
    = ((mid, idx), (modm, srcm, gen))
      where
        mid = fromMaybe 0 $ m_lookup cmpmodName d modm
        idx = fromMaybe 0 $ m_lookup cmpmodName d srcm $mb_bind elemIndex cmpmodName n
        
makeMod :: (modName, char, modId, [modName]) -> state parseSt module
makeMod (n, c, mid, dests)
    = st_mapM (mkDstId n) dests >>= mkMod
      where
        mkMod dsts'
            = go c
              where              
                go 'b' = st_pure ("broadcaster", mid, Broadcaster, dsts') 

                go '%' = mkFF <$> allocPulseStId 1
                         where
                           mkFF pid = (n, mid, (FlipFlop pid), dsts')

                go '&' = getSrcs n >>= (alloc . length)
                         where
                           alloc sz      = mkConj sz <$> allocPulseStId sz
                           mkConj sz pid = (n, mid, (Conjunction pid sz), dsts')

                go _   = error "bad mod type character"


|| running
|| the run state consists of the module and pulse vectors, along with a dequeue holding the
|| signals to send.  Running consists of sending each signal in turn, collecting other signals
|| to be sent, until there are no signals left to send.  Running is performed in the st monad
|| to allow mutable updates to the pulseSt vector

rnSt == (net, vector module, mvector pulse, dequeue (modId, signal), dequeue pulse, int)

|| added because dequeue doesn't export a cmpdequeue function, so can't auto-derive
cmprnSt = undef

|| lenses for rnSt
rn_net  = lensTup6_0
rn_vm   = lensTup6_1
rn_vp   = lensTup6_2
rn_sigq = lensTup6_3
rn_sent = lensTup6_4
rn_push = lensTup6_5

|| lift lens operations into rnSt
rn_view :: lens rnSt * -> state rnSt *
rn_view lns st = (view lns st, st)

rn_set :: lens rnSt * -> * -> state rnSt ()
rn_set lns v st = ((), set lns v st)

rn_over :: lens rnSt * -> (* -> *) -> state rnSt ()
rn_over lns f st = ((), over lns f st)


send :: pulse -> (modId, pulseStId) -> state rnSt ()
send p (mid, idx) = rn_over rn_sent (dq_addR p) >> rn_over rn_sigq (dq_addR (mid, (idx, p)))

exec :: module -> signal -> state rnSt ()
exec (n, mid, mt, dests) (idx, p)
    = go mt
      where
        go Broadcaster = st_mapM_ (send p) dests

        go (FlipFlop pid)
            = st_pure (),         if isHigh p
            = v_flip >>= doSends, otherwise
              where
                doSends p = st_mapM_ (send p) dests

                v_flip
                    = rn_view rn_vp >>= f
                      where
                        f vp
                            = v_read vp pid >>= update
                              where
                                update p = st_pure p' << v_write vp pid p' where p' = p_not p

        go (Conjunction pid len)
            = st_bind2 (rn_view rn_push) doUpdateReads doSends
              where
                doUpdateReads
                    = rn_view rn_vp >>= f
                      where
                        f vp = v_write vp (pid + idx) p >> st_mapM (v_read vp) [pid .. pid + len - 1]

                doSends bps ps
                     = st_mapM_ (send p') dests
                       where
                         p' = Low,  if all isHigh ps
                            = High, otherwise

pushButton :: state rnSt ()
pushButton
    = rn_over rn_push (+ 1) >> (getBroadcaster >>= send Low) >> runToIdle
      where
        getBroadcaster = (($pair 0) . view mod_id . fromJust . m_lookup cmpmodName "broadcaster") <$> rn_view rn_net

        runToIdle
            = rn_view rn_sigq >>= check
              where
                check q
                    = st_pure (),                                    if dq_null q
                    = rn_set rn_sigq q' >> rn_view rn_vm >>= doExec, otherwise
                      where
                        ((mid, sig), q') = fromJust $ dq_viewL q
                        doExec vm        = exec (vm !! mid) sig >> runToIdle

warmUp :: rnSt -> int
warmUp st
    = #hi * #lo
      where
        sent = st_evalState (st_mapM_ (const pushButton) [1 .. 1000] >> rn_view rn_sent) st
        (hi, lo) = partition isHigh . dq_toList $ sent

|| part 2: need to find out when a single low-pulse is sent to rx
|| rx is the output of %zh, which is a conj of xc, th, pd, bp
|| so need to find the periods between low-pulse sends for xc, th, pd, bp, and find the lcm
doPart2 :: rnSt -> int
doPart2 st
    = foldl1 lcm [3823, 3877, 4001, 3847]       || periods determined by tracing inspection

readNet :: string -> io rnSt
readNet fn
    = (go . lines) <$>. readFile fn
      where
        go specs
            = (net, vm, (v_unsafeThaw vp), dq_empty, dq_empty, 0)
              where
                initSt = (m_empty, m_empty, 0)
                (pmods, (modm, srcm, _   )) = st_runState (st_mapM parseMod specs) initSt
                (mods,  (_,    _,    gen')) = st_runState (st_mapM makeMod pmods) (modm, srcm, 0)

                net  = m_fromList cmpmodName $ zip2 (map (view mod_name) mods) mods
                vm   = v_fromList mods
                vp   = v_rep gen' Low

day20 :: io ()
day20
    = readNet "../inputs/day20.txt" >>=. go
      where
        go st
            = io_mapM_ putStrLn [part1, part2]
              where
                part1 = ("part 1: " ++) . showint . warmUp $ st
                part2 = ("part 2: " ++) . showint . doPart2 $ st
