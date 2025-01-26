|| day19.m


%export +       || most parts are re-used by day21

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import <parser> (<$>)/p_fmap (<*)/p_left (*>)/p_right
%import <vector>
%import <state> (>>=)/st_bind (<$$>)/st_fmap (>>)/st_right


registers   == vector int
mregisters  == mvector int
instruction == mregisters -> st ()
operation   == int -> int -> int -> instruction
program     == vector instruction

operationMap :: m_map string operation
operationMap
    = m_fromList cmpstring
        [ ("addr", op_rr (+))
        , ("addi", op_ri (+))
        , ("mulr", op_rr (*))
        , ("muli", op_ri (*))
        , ("banr", op_rr (.&.))
        , ("bani", op_ri (.&.))
        , ("borr", op_rr (.|.))
        , ("bori", op_ri (.|.))
        , ("setr", op_ri const)
        , ("seti", op_ii const)
        , ("gtir", op_ir (boolVal (>)))
        , ("gtri", op_ri (boolVal (>)))
        , ("gtrr", op_rr (boolVal (>)))
        , ("eqir", op_ir (boolVal (==)))
        , ("eqri", op_ri (boolVal (==)))
        , ("eqrr", op_rr (boolVal (==)))
        ]

boolVal :: (int -> int -> bool) -> int -> int -> int
boolVal p a b = if' (p a b) 1 0

|| Note: these functions are written partially curried, with the op a b c provided during the parsing phase, and
|| the regs and s provided during execution of the individual instruction.  The state s is passed explicitly and
|| provided to st_runState because the compiler has no context from where this is called to do the inlining required
|| to get rid of over-applications of state, so we do it manually
op_rr, op_ri, op_ir, op_ii :: (int -> int -> int) -> operation
op_rr op a b c = go where go regs s = st_runState (st_bind2 (v_unsafeRead regs a) (v_unsafeRead regs b) go) s where go a' b' = v_unsafeWrite regs c (a' $op b')
op_ri op a b c = go where go regs s = st_runState (v_unsafeRead regs a >>= go) s where go a' = v_unsafeWrite regs c (a' $op b)
op_ir op a b c = go where go regs s = st_runState (v_unsafeRead regs b >>= go) s where go b' = v_unsafeWrite regs c (a $op b')
op_ii op a b c = go where go regs s = st_runState (v_unsafeWrite regs c (a $op b)) s

step :: (int -> mregisters -> st bool) -> (int, program) -> mregisters -> st bool
step haltTest (ipReg, prog) mregs
    = v_unsafeRead mregs ipReg >>=
      fetch                    >>=
      exec
      where
        fetch ip
            = check <$$> haltTest ip mregs
              where
                check doHalt
                    = Nothing,           if doHalt \/ ip < 0 \/ ip >= v_length prog
                    = Just (prog !! ip), otherwise

        exec Nothing = st_pure True
        exec (Just inst)
            = inst mregs                 >>
              v_modify mregs (+ 1) ipReg >>
              st_pure False

        
runUntil :: (int -> mregisters -> st bool) -> (int, program) -> mregisters -> st mregisters
runUntil haltTest info mregs
    = run' False
      where
        run' False = step haltTest info mregs >>= run'
        run' True  = st_pure mregs

run :: (int, program) -> registers -> registers
run info regs
    = st_evalState (runUntil noHalt info (v_unsafeThaw regs) >>= v_unsafeFreeze) ()
      where
        || note: noHalt written with explicit state and st_runState, since it is passed as a parameter
        || to the higher-order function runUntil, and the compiler can't figure out at the call site
        || that it will be called with state
        noHalt _ _ s = (False, s)

p_val :: parser int
p_val = p_int <* p_spaces

p_opName :: parser string
p_opName = p_some p_letter <* p_spaces

p_ipReg :: parser int
p_ipReg = p_string "#ip" *> p_spaces *> p_val

p_inst :: parser instruction
p_inst
    = p_liftA4 mkInst p_opName p_val p_val p_val
      where
        mkInst s a b c
            = op a b c
              where
                op = fromJust $ m_lookup cmpstring s operationMap

p_prog :: parser (int, program)
p_prog
    = p_liftA2 mkProg p_ipReg (p_some p_inst)
      where
        mkProg ipReg insts = (ipReg, v_fromList insts)

factorSum :: int -> int
factorSum n
    = foldl go 0 [1 .. n]
      where
        go s m
            = s + m, if n $mod m == 0
            = s,     otherwise

readProgram :: string -> io (int, program)
readProgram fn
    = go <$>. parse p_prog <$>. readFile fn
      where
        go (ms, ps) = fromMaybe (error (p_error ps)) ms

day19 :: io ()
day19
    = readProgram "../inputs/day19.input" >>=. go
      where
        go info
            = io_mapM_ putStrLn [part1, part2]
              where
                value = 10551277
                part1 = (++) "part 1: " . showint . v_first . run info $ v_rep 6 0
                part2 = (++) "part 2: " . showint . factorSum $ value
