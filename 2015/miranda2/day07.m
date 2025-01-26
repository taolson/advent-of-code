|| day07.m


%export day07

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <either>
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import <parser> (*>>=)/p_bind (<$>)/p_fmap (<*)/p_left (*>)/p_right (<|>)/p_alt
%import <state> (>>=)/st_bind (>>)/st_right


op ::= WIRE | NOT | AND | OR | LSHIFT | RSHIFT

signal  == either int string
gate    == (signal, signal, op)
sigval  == either int gate
circuit == m_map string sigval

getValue :: signal -> state circuit int
getValue (Left x) = st_pure x
getValue (Right s)
    = st_get >>= (st_pure . fromJust . m_lookup cmpstring s) >>= go
      where
        go (Left x) = st_pure x
        go (Right (s1, s2, op))
            = st_liftA2 (doOp op) (getValue s1) (getValue s2) >>= update
              where
                doOp WIRE   a _ = a
                doOp NOT    a _ = complement a
                doOp AND    a b = a .&.  b
                doOp OR     a b = a .|.  b
                doOp LSHIFT a b = a .<<. b
                doOp RSHIFT a b = a .>>. b

        update x = st_modify (m_insert cmpstring s (Left x)) >> st_pure x


p_signal :: parser signal
p_signal = ((Left <$> p_int) <|> (Right <$> p_word)) <* p_spaces

p_notv :: parser sigval
p_notv
    = mkNot <$> (p_string "NOT " *> p_signal)
      where
        mkNot s = Right (s, Left 0, NOT)

p_wire :: parser sigval
p_wire
    = mkWire <$> p_signal
      where
        mkWire (Left x) = Left x
        mkWire s        = Right (s, Left 0, WIRE)

p_op :: parser op
p_op = (p_word <* p_spaces) *>>= mkOp
       where
         mkOp "AND"    = p_pure AND
         mkOp "OR"     = p_pure OR
         mkOp "LSHIFT" = p_pure LSHIFT
         mkOp "RSHIFT" = p_pure RSHIFT
         mkOp _        = p_fail

p_binop :: parser sigval
p_binop
    = p_liftA3 mkBinop p_signal p_op p_signal
      where
        mkBinop s1 op s2 = Right (s1, s2, op)

p_entry :: parser (string, sigval)
p_entry
    = p_liftA2 mkEntry (p_binop <|> p_notv <|> p_wire) (p_string "-> " *> p_word) <* p_spaces
      where
        mkEntry val sig = (sig, val)        


readCircuit :: string -> io circuit
readCircuit fn
    = go <$>. parse (p_some p_entry <* p_end) <$>. readFile fn
      where
        go (mentries, ps) = fromMaybef (error (p_error ps)) (m_fromList cmpstring) mentries

day07 :: io ()
day07
    = readCircuit "../inputs/day07.input" >>=. go
      where
        go c1
            = io_mapM_ putStrLn [part1, part2]
              where
                eval  = st_evalState (getValue (Right "a"))
                a1    = eval c1
                c2    = m_insert cmpstring "b" (Left a1) c1
                a2    = eval c2
                part1 = (++) "part 1: " . showint $ a1
                part2 = (++) "part 2: " . showint $ a2
