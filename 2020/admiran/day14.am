|| day14.m


%export day14

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import <parser> (<$>)/p_fmap (<*>)/p_apply (<*)/p_left (*>)/p_right (<|>)/p_alt


instruction ::=  Mem int int | Mask int int    
    
p_memInst :: parser instruction
p_memInst = p_liftA2 Mem (p_string "mem[" *> p_int) (p_string "] = " *> p_int) <* p_spaces

p_maskInst :: parser instruction
p_maskInst
    = buildMask <$> (p_string "mask = " *> p_some (p_digit <|> p_char 'X')) <* p_spaces
      where
        buildMask             = makeMask . foldl addDigit (1, 0)
        makeMask (m0, m1)     = Mask m0 m1
        addDigit (m0, m1) 'X' = ((m0 .<<. 1) .|. 1, (m1 .<<. 1) .|. 0)
        addDigit (m0, m1) '0' = ((m0 .<<. 1) .|. 0, (m1 .<<. 1) .|. 0)
        addDigit (m0, m1) '1' = ((m0 .<<. 1) .|. 1, (m1 .<<. 1) .|. 1)
        addDigit _        _   = error "p_maskInst: bad parse"

p_program :: parser [instruction]
p_program = p_some (p_memInst <|> p_maskInst)

processor ::= Processor (m_map int int) int int

getMem :: processor -> m_map int int
getMem (Processor mem _ _) = mem

initProc :: processor
initProc = Processor m_empty (-1) 0

run1 :: processor -> [instruction] -> processor
run1
    = foldl exec
      where
        exec (Processor mem _  _)  (Mask m0 m1) = Processor mem m0 m1
        exec (Processor mem m0 m1) (Mem a d)    = Processor (m_insert cmpint a ((d .&. m0) .|. m1) mem) m0 m1
        exec _                     _            = undef || to get rid of compiler warning

run2 :: processor -> [instruction] -> processor
run2
    = foldl exec
      where
        exec (Processor mem _  _)  (Mask m0 m1) = Processor mem m0 m1
        exec (Processor mem m0 m1) (Mem a d)
            = Processor (foldl (setMem d) mem $ makeAddrs 36 a m0 m1) m0 m1
              where
                setMem d m a = m_insert cmpint a d m
        exec _                     _            = undef || to get rid of compiler warning


makeAddrs :: int -> int -> int -> int -> [int]
makeAddrs n a m0 m1
    = [0], if n == 0
    = map (insertBit a0) as, if b0 == 0
    = as1,                   if b1 == 1
    = as0 ++ as1,            otherwise
      where
        a0  = a  .&. 1
        b0  = m0 .&. 1
        b1  = m1 .&. 1
        as  = makeAddrs (n - 1) (a .>>. 1) (m0 .>>. 1) (m1 .>>. 1)
        as0 = map (insertBit 0) as
        as1 = map (insertBit 1) as

        insertBit b a = (a .<<. 1) .|. b

readProgram :: string -> io [instruction]
readProgram fn
    = go <$>. parse p_program <$>. readFile fn
      where
        go (mprog, ps) = fromMaybe (error (p_error ps)) mprog

day14 :: io ()
day14
    = readProgram "../inputs/day14.txt" >>=. go
      where
        go prog
            = io_mapM_ putStrLn [part1, part2]
              where
                process f = showint . sum . m_elems . getMem . f initProc $ prog
                part1     = "part 1: " ++ process run1
                part2     = "part 2: " ++ process run2
