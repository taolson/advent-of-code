%export day21

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <map>
%import <maybe>
%import <parser>
%import <mirandaExtensions>


monkey == string

expr ::= Int int | Monkey monkey | Op char expr expr

exprMap == m_map monkey expr

p_monkey :: parser monkey
p_monkey = p_some p_letter $p_left p_spaces

p_op :: parser char
p_op = p_any $p_left p_spaces
     
p_intExpr, p_opExpr, p_expr :: parser expr
p_intExpr = Int $p_fmap (p_posint $p_left p_spaces)

p_opExpr
    = p_liftA3 mkOp p_monkey p_op p_monkey
      where
        mkOp m1 op m2 = Op op (Monkey m1) (Monkey m2)

p_expr = p_intExpr $p_alt p_opExpr

p_entry :: parser (monkey, expr)
p_entry = p_liftA2 pair (p_monkey $p_left p_string ": ") p_expr

eval :: exprMap -> expr -> int
eval t
    = go
      where
        go (Int n)       = n
        go (Monkey m)    = (go . fromJust . m_lookup cmpmonkey m) t
        go (Op op e1 e2) = ex op (go e1) (go e2)

        ex '+' = (+)
        ex '-' = (-)
        ex '*' = (*)
        ex '/' = div
        ex _   = error "bad ex"

hasMonkey :: exprMap -> monkey -> expr -> bool
hasMonkey t h
    = go
      where
        go (Int n) = False

        go (Monkey m)
            = True,                           if _eq cmpmonkey m h
            = (go . fromJust . m_lookup cmpmonkey m) t, otherwise

        go (Op op e1 e2) = go e1 \/ go e2


|| part 2: need to figure out what intber humn should return so that root's two monkey names evaluate equal
|| need to rewrite e1 == e2 to humn = e'
|| first, make humn appear in the lhs
|| then proceed to undo the left expr and replicate the opposite op in the right expr
|| e.g.
||   a + b * humn = 3 - d -> b * humn = (3 - d) + a -> humn = ((3 - d) + a) / b
solve :: exprMap -> monkey -> monkey -> int
solve t h r
    = go e1 e2, if hasMonkey t h e1
    = go e2 e1, otherwise
      where
        Op _ e1 e2 = fromJust (m_lookup cmpmonkey r t)

        go (Monkey m) rhs
            = eval t rhs, if _eq cmpmonkey m h
            = go e rhs,   otherwise
              where
                e = fromJust (m_lookup cmpmonkey m t)

        go (Op op e1 e2) rhs
            = go e1 (inv op rhs e2),  if hasMonkey t h e1
            = go e2 (conv op rhs e1), otherwise

        go e1 e2 = error ("solve: bad lhs expr: " ++ showexpr e1)

        || invert the operation
        inv '+' = Op '-'
        inv '-' = Op '+'
        inv '*' = Op '/'
        inv '/' = Op '*'
        inv _   = error "bad invert"

        || commutative ops invert the operation,
        || anti-commutative ops flip the operands
        conv '+' = Op '-'
        conv '-' = converse (Op '-')
        conv '*' = Op '/'
        conv '/' = converse (Op '/')
        conv _   = error "bad conv"

readTree :: string -> io exprMap
readTree fn
    = go <$>. parse (p_some p_entry) <$>. readFile fn
      where
        go (pr, ps)
            = fromMaybef (error (p_error ps)) (m_fromList cmpmonkey) pr

day21 :: io ()
day21
    = readTree "../inputs/day21.txt" >>=. go
      where
        go tree
            = io_mapM_ putStrLn [part1, part2]
              where
                part1 = (++) "part 1: " . showint . eval tree . Monkey $ "root"
                part2 = (++) "part 2: " . showint . solve tree "humn"  $ "root"
