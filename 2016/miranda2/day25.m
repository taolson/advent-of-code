|| day25.m


%export day25

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <either>
%import <maybe>
%import <mirandaExtensions>
%import <parser> (*>>=)/p_bind (<$>)/p_fmap (<*)/p_left (*>)/p_right (<|>)/p_alt
%import <state> (>>=)/st_bind (>>)/st_right
%import <vector>


reg   == int
regId == int
genOp == either regId int

instruction ::= 
    Cpy genOp genOp     |
    Inc genOp           |
    Dec genOp           |
    Jnz genOp genOp     |
    Ret genOp           |
    Out genOp

program    == vector  instruction
mprogram   == mvector instruction
registers  == vector  reg
mregisters == mvector reg

run :: program -> registers -> int -> int -> [int]
run prog regs pc outLimit
    = st_evalState (step pc 0 []) ()
      where
        mprog    = v_thaw prog
        mregs    = v_thaw regs
        mprogLen = v_mlength mprog

        step pc outCount output
            = st_pure output,                 if pc < 0 \/ pc >= mprogLen \/ outCount >= outLimit
            = v_unsafeRead mprog pc >>= exec, otherwise
              where
                pc1 = pc + 1

                getValue (Left a)  = v_unsafeRead mregs a
                getValue (Right a) = st_pure a

                update (Left a) x  = v_unsafeWrite mregs a x
                update _        _  = error "run: update called with Right value"

                stepWithPc pc      = step pc outCount output

                exec (Cpy src dst) = (getValue src >>= update dst)                   >> stepWithPc pc1
                exec (Inc reg)     = (getValue reg >>= (update reg . (+ 1)))         >> stepWithPc pc1
                exec (Dec reg)     = (getValue reg >>= (update reg . (subtract 1)))  >> stepWithPc pc1
                exec (Out reg)     =  getValue reg >>=                                  step pc1 (outCount + 1) . (: output)
                exec (Ret reg)     = st_pure output

                exec (Jnz tst dst)
                    = getValue tst >>= check
                      where
                        check 0 = stepWithPc pc1
                        check _ = getValue dst >>= (stepWithPc . (pc +))

searchClock :: program -> int
searchClock prog
    = fromJust . find (isClock . runWithVal) $ [0 ..]
      where
        runWithVal v = run prog (v_fromList [v, 0, 0, 0]) 0 16
        isClock      = and . zipWith (==) (cycle [1, 0])

p_reg :: parser reg
p_reg = mkReg <$> (p_spaces *> p_letter) where mkReg c = code c - code 'a'

p_sint :: parser int
p_sint = p_spaces *> p_int

p_gen :: parser genOp
p_gen = (Left <$> p_reg) <|> (Right <$> p_sint)

p_cpy, p_inc, p_dec, p_jnz, p_ret, p_out, p_inst :: parser instruction
p_inc = Inc <$> p_gen
p_dec = Dec <$> p_gen
p_ret = Ret <$> p_gen
p_out = Out <$> p_gen
p_cpy = p_liftA2 Cpy p_gen p_gen
p_jnz = p_liftA2 Jnz p_gen p_gen

p_inst
    = p_spaces *> (p_word *>>= doInst) <* p_char '\n'
      where
        doInst "cpy" = p_cpy
        doInst "inc" = p_inc
        doInst "dec" = p_dec
        doInst "jnz" = p_jnz
        doInst "ret" = p_ret
        doInst "out" = p_out
        doInst _     = p_fail
       
readProgram :: string -> io program
readProgram fn
    = go <$>. parse (p_some p_inst) <$>. readFile fn
      where
        go (mprog, ps) = fromMaybef (error (p_error ps)) v_fromList mprog

day25 :: io ()
day25
    = readProgram "../inputs/day25.input" >>=. go
      where
        go prog = putStrLn . (++) "part 1: " . showint . searchClock $ prog
