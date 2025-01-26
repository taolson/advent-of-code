|| day23.m


%export day23

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <lens>
%import <maybe>
%import <mirandaExtensions>
%import <parser> (*>)/p_right (<|>)/p_alt
%import <state> (>>=)/st_bind (>>)/st_right
%import <vector>


reg == int

opr ::= Reg reg | Imm int

instruction ::=
    Set reg opr |
    Sub reg opr |
    Mul reg opr |
    Mod reg opr |
    Jnz opr opr

program   == vector instruction
registers == mvector reg

runState ::= Run | Halt | Wait

processor == (runState, registers, int, int)


|| lenses for processor
p_rs    = lensTup4_0
p_regs  = lensTup4_1
p_pc    = lensTup4_2
p_muls  = lensTup4_3

|| lift lenses into state
st_view lns   proc = (view lns proc, proc)
st_set  lns x proc = ((), set lns x proc)
st_over lns f proc = ((), over lns f proc)

running :: processor -> bool
running p
    = case view p_rs p of
        Run -> True
        _   -> False

getReg :: reg -> state processor int
getReg r proc = v_read (view p_regs proc) r proc

getOpr :: opr -> state processor int
getOpr (Imm n) = st_pure n
getOpr (Reg r) = getReg r

setReg :: reg -> int -> state processor ()
setReg r n proc = v_write (view p_regs proc) r n proc

initProc :: processor
initProc = (Run, v_unsafeThaw (v_rep 26 0), 0, 0)

exec :: instruction -> state processor ()
exec (Set d s) = (getOpr s >>= setReg d)           >> st_pure ()
exec (Sub d s) = st_bind2 (getReg d) (getOpr s) op >> st_pure ()                                  where op a b = setReg d (a - b)
exec (Mul d s) = st_bind2 (getReg d) (getOpr s) op >> st_modify (over p_muls (+ 1)) >> st_pure () where op a b = setReg d (a * b)
exec (Mod d s) = st_bind2 (getReg d) (getOpr s) op >> st_pure ()                                  where op a b = setReg d (a $mod b)

exec (Jnz d s)
    = st_bind2 (getOpr d) (getOpr s) jmp
      where
        jmp a b = st_over p_pc (+ b - 1) >>     || subtract 1, because we already incremented pc by default
                  st_pure (), if a ~= 0
                = st_pure (), otherwise

step :: program -> state processor ()
step prog
    = st_bind2 (st_view p_rs) (st_view p_pc) go
      where
        go Run pc
            = st_set p_rs Halt   >> st_pure (),        if pc < 0 \/ pc >= v_length prog
            = st_over p_pc (+ 1) >> exec (prog !! pc), otherwise

        go _ _ = st_pure ()

run :: program -> processor -> processor
run prog proc
    = proc,                      if ~running proc
    = proc' $seq run prog proc', otherwise
      where
        proc' = st_execState (step prog) proc

p_instruction :: parser instruction
p_instruction
    = p_ro "set" Set <|>
      p_ro "sub" Sub <|>
      p_ro "mul" Mul <|>
      p_ro "mod" Mod <|>
      p_oo "jnz" Jnz
      where
        p_ro s op = p_liftA2 op (p_string s *> p_spaces *> p_reg) (p_spaces *> p_opr)
        p_oo s op = p_liftA2 op (p_string s *> p_spaces *> p_opr) (p_spaces *> p_opr)

        p_reg = (subtract (code 'a') . code) $p_fmap p_letter
        p_opr = (Reg $p_fmap p_reg) <|> (Imm $p_fmap p_int)
        
|| integer sqrt, babylonian method
isqrt :: int -> int
isqrt n
    = go n 1
      where
        go x y
            = x,        if x <= y
            = go x' y', otherwise
              where
                x' = (x + y) $div 2
                y' = n $div x'

isPrime n
    = all notDiv . takeWhile (<= isqrt n) $ primes
      where
        notDiv d = n $mod d ~= 0

primes = 2 : 3 : filter isPrime [5, 7 ..]

compositesInRange :: int -> int -> int -> int
compositesInRange lo hi step
    = sum . map compTest $ [lo, lo + step .. hi]
      where
        compTest n = if' (isPrime n) 0 1

readProgram :: string -> io program
readProgram fn
    = go <$>. parse (p_someSepBy (p_char '\n') p_instruction) <$>. readFile fn
      where
        go (mprog, ps) = fromMaybef (error (p_error ps)) v_fromList mprog

day23 :: io ()
day23
    = readProgram "../inputs/day23.input" >>=. go
      where
        go prog
            = io_mapM_ putStrLn [part1, part2]
              where
                part1 = (++) "part 1: " . showint . view p_muls . run prog $ initProc
                part2 = (++) "part 2: " . showint $ compositesInRange 109300 (109300 + 17000) 17
