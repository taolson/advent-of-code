|| day08.m


%export day08

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <lens>
%import <maybe>
%import <mirandaExtensions>
%import <parser> (<$>)/p_fmap (<*>)/p_apply (<*)/p_left (*>)/p_right (<|>)/p_alt
%import <set>
%import <vector>

instruction ::= Nop int | Acc int | Jmp int

program == vector instruction

p_pInt, p_nInt, p_sInt :: parser int
p_pInt = p_char '+' *> p_posint
p_nInt = neg <$> (p_char '-' *> p_posint)
p_sInt = p_pInt <|> p_nInt

p_opc :: parser string
p_opc = p_some p_letter

p_instr :: parser instruction
p_instr
    = p_liftA2 makeInstr (p_opc <* p_spaces) (p_sInt <* p_spaces)
      where
        makeInstr "nop" = Nop
        makeInstr "acc" = Acc
        makeInstr "jmp" = Jmp
        makeInstr s     = error "p_instr: bad instruction"

runState ::= Run | Loop | Done

patch == (int, instruction)

procState == (runState, s_set int, patch, int, int)

|| lenses for procState
ps_rs    = lensTup5_0
ps_seen  = lensTup5_1
ps_patch = lensTup5_2
ps_pc    = lensTup5_3
ps_acc   = lensTup5_4

initState :: procState
initState = (Run, s_empty, (-1, Nop 0), 0, 0)

run :: program -> procState -> procState
run prog st
    = set ps_rs Done st,          if pc >= v_length prog
    = set ps_rs Loop st,          if s_member cmpint pc seen
    = go (fetchInst ptch pc) st,  otherwise
      where
        pc    = view ps_pc st
        pc'   = pc + 1
        seen  = view ps_seen st
        seen' = s_insert cmpint pc seen
        ptch  = view ps_patch st

        go (Nop n) st = run prog . set ps_seen seen' . set ps_pc pc' $ st
        go (Acc n) st = run prog . set ps_seen seen' . set ps_pc pc' . over ps_acc (+ n) $ st
        go (Jmp n) st = run prog . set ps_seen seen' . over ps_pc (+ n) $ st

        fetchInst (ppc, pinst) pc
            = pinst,      if ppc == pc
            = prog !! pc, otherwise

modify :: program -> int -> procState -> procState
modify prog pc st
    = case prog !! pc of
        Nop n     -> set ps_patch (pc, Jmp n) st
        Acc n     -> st
        Jmp n     -> set ps_patch (pc, Nop n) st

findDone :: program -> procState
findDone prog
    = hd . dropWhile (notDone . view ps_rs) . map runPatched $ [0 .. v_length prog - 1]
       where
         runPatched pc = run prog $ modify prog pc initState
         notDone Done  = False
         notDone _     = True

readProgram :: string -> io program
readProgram fn
    = go <$>. parse (p_some p_instr) <$>. readFile fn
      where
        go (minstrs, ps) = fromMaybef (error (p_error ps)) v_fromList minstrs

day08 :: io ()
day08
    = readProgram "../inputs/day08.txt" >>=. go
      where
        go prog
            = io_mapM_ putStrLn [part1, part2]
              where
                st    = run prog initState
                part1 = (++) "part 1: " . showint $ view ps_acc st
                part2 = (++) "part 2: " . showint . view ps_acc . findDone $ prog
