%export day23

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import "intcode"


packet ::= Packet
           num  || addr
           num  || x
           num  || y

p_addr :: packet -> num
p_addr (Packet a x y) = a

p_y :: packet -> num
p_y (Packet a x y) = y

set_addr :: num -> packet -> packet
set_addr a' (Packet a x y) = Packet a' x y

showjitState = undef
cmpjitState  = undef

nic ::= NIC
        jitState       || task
        [packet]       || queue
        (maybe packet) || out

makeNIC :: program -> num -> nic
makeNIC prog id = NIC task [] Nothing where task = ((converse jitPutInput) id . jitReset) prog

n_out :: nic -> maybe packet
n_out (NIC task queue out) = out

runNIC :: nic -> nic
runNIC (NIC task queue out)
    = NIC task3 queue2 out2
      where
        task1         = jitContinue task
        (inp, queue2) = ([-1],   queue),  if null queue
                      = ([x, y], queue1), otherwise
                        where
                          Packet _ x y : queue1 = queue
       task2          = jitPutAllInput task1 inp
       (out2, task3)  = (Nothing, task2),                                        if null out
                      = (Just (Packet a x y), JitState rs ip rb mem inp out1 c), otherwise
                         where
                           JitState rs ip rb mem inp out c = task2
                           (a : x : y : out1)              = out

nicStats :: nic -> (num, num, num)
nicStats (NIC task queue out) = jitGetCacheStats task

network == m_map num nic

sendPacket :: network -> packet -> network
sendPacket net packet
    = m_adjust cmpint (addPacket packet) addr net
      where
        addr                         = p_addr packet
        addPacket packet (NIC t q o) = NIC t (q ++ [packet]) o

runNetwork :: network -> (network, maybe packet)
runNetwork
    = go 0
      where
      go idle net
          = (net2, natPacket),  if isJust natPacket \/ idle >= 5
          = go (idle + 1) net1, if null packets
          = go 0          net2, otherwise
            where
              net1      = m_fmap runNIC net
              packets   = (catMaybes . map (n_out . snd) . m_toList) net1
              net2      = foldl sendPacket net1 packets
              natPacket = find ((== 255) . p_addr) packets

netStats :: network -> (num, num, num)
netStats net
    = foldl addStats (0, 0, 0) (map nicStats (m_elems net))
      where
        addStats (ac, hi, fl) (ac', hi', fl') = (ac + ac', hi + hi', fl + fl')

nat ::= NAT
        network         || net
        (maybe packet)  || received
        (maybe packet)  || sent

runNAT :: nat -> (network, packet)
runNAT (NAT net m_rcv m_snt)
    = (net, rcv),                      if trigger & matchYs m_rcv m_snt
    = runNAT (NAT net2 m_rcv' m_snt'), otherwise
      where
        (net1, m_np) = runNetwork net
        trigger      = isNothing m_np
        m_rcv'       = Nothing,                          if trigger
                     = m_np $mb_alt m_rcv,               otherwise
        m_snt'       = m_rcv $mb_alt m_snt,              if trigger
                     = m_snt,                            otherwise
        rcv          = fromJust m_rcv
        net2         = sendPacket net1 (set_addr 0 rcv), if trigger
                     = net1,                             otherwise
        matchYs (Just (Packet a1 x1 y1)) (Just (Packet a2 x2 y2)) = y1 == y2
        matchYs a b  = False

day23 :: io ()
day23
    = readProgram "../inputs/day23.input" >>=. go
      where
        go prog
            = io_mapM_ putStrLn [part1, part2, stats]
              where
                net          = foldl addNIC m_empty [0 .. 49] where  addNIC net id = m_insert cmpint id (makeNIC prog id) net
                (_, Just p1) = runNetwork net
                (net', p2)   = runNAT (NAT net Nothing Nothing)
                part1        = (++) "part 1: " . showint . p_y $ p1
                part2        = (++) "part 2: " . showint . p_y $ p2
                stats        = "accesses: " ++ showint ac ++ " hits: " ++ showint hi ++ " flushes: " ++ showint fl
                               where (ac, hi, fl) = netStats net'
