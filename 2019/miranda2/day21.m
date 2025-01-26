%export day21

%import <io> (>>=.)/io_bind
%import <maybe>
%import <mirandaExtensions>
%import <parser> (<$>)/p_fmap (>>)/p_right (<<)/p_left (<|>)/p_alt
%import "intcode"


binop ::= And | Or

deMorgan :: binop -> binop
deMorgan And = Or
deMorgan Or  = And

showOp :: binop -> string
showOp And = "AND"
showOp Or  = "OR"

expr  ::= Var char! | Not expr! | Opr binop! expr! expr!

optimize :: expr -> expr
optimize (Not (Not a))            = optimize a
optimize (Not a)                  = Not a',            if _eq cmpexpr a' a
                                  = optimize (Not a'), otherwise
                                    where
                                      a' = optimize a
optimize (Opr op (Not a) (Not b)) = optimize (Not (Opr (deMorgan op) (optimize a) (optimize b)))
optimize (Opr op a       b)       = Opr op a' b',            if _eq cmpexpr a' a & _eq cmpexpr b' b
                                  = optimize (Opr op a' b'), otherwise
                                    where
                                      a' = optimize a
                                      b' = optimize b
optimize e                        = e

script == [string]

generate :: string -> expr -> script
generate tgt (Var v)                  = ["OR " ++ [v] ++ " " ++ tgt]       || only works if it's the first insn to generate to tgt
generate tgt (Not (Var v))            = ["NOT " ++ [v] ++ " " ++ tgt]
generate tgt (Not e)                  = generate tgt e ++ ["NOT " ++ tgt ++ " " ++ tgt]
generate tgt (Opr op e1      (Var v)) = generate tgt e1 ++ [showOp op ++ " " ++ [v] ++ " " ++ tgt]
generate tgt (Opr op (Var v) e2)      = generate tgt e2 ++ [showOp op ++ " " ++ [v] ++ " " ++ tgt]
generate tgt (Opr op e1      e2)      = generate (altReg tgt) e1 ++ generate tgt e2 ++ [showOp op ++ " " ++ altReg tgt ++ " " ++ tgt]
                                        where
                                          altReg "T" = "J"
                                          altReg "J" = "T"
                                          altReg _   = error "altReg: bad tgt"

runScript :: script -> string -> program -> [num]
runScript script cmd prog
    = out
      where
        inp       = map code (intercalate "\n" script ++ "\n" ++ cmd ++ "\n")
        (out, _)  = (jitGetAllOutput . jitRun prog) inp

|| parsing

p_inParens :: parser * -> parser *
p_inParens p = p_char '(' >> p_spaces >> p << p_spaces << p_char ')'

p_term :: parser expr
p_term = (Var <$> p_letter)               <|>
         (Not <$> (p_char '!' >> p_term)) <|>
         p_inParens p_expr

p_expr :: parser expr
p_expr
    = (p_liftA2 (Opr And) (p_term << p_string " & ") p_expr) <|>
      (p_liftA2 (Opr Or)  (p_term << p_string " | ") p_expr) <|>
      p_term

parseScript :: string -> expr
parseScript s
    = error (p_error ps), if isNothing me
    = fromJust me,        otherwise
      where
        (me, ps) = parse (p_expr << p_end) s

day21 :: io ()
day21
    = readProgram "../inputs/day21.input" >>=. go
      where
        go prog
            = io_mapM_ putStrLn [part1, part2]
              where
                script1 = makeScript "(!A | !B | !C) & D"
                script2 = makeScript "(!A | !B | !C) & (E | H) & D"
                part1   = "part 1: " ++ process script1 "WALK"
                part2   = "part 2: " ++ process script2 "RUN"

                makeScript = generate "J" . optimize . parseScript

                process s t
                    = showint damage, if damage > 127
                    = err,            otherwise
                      where
                        out    = runScript s t prog
                        damage = last out
                        err    = map decode out                
