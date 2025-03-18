|| day24 -- Arithmetic Logic Unit


%import <io>                    (>>=)/io_bind
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import <vector>


|| instruction operand is reg w, x, y, z or literal int
op ::= W | X | Y | Z | I int

parseOp :: string -> op
parseOp "w" = W
parseOp "x" = X
parseOp "y" = Y
parseOp "z" = Z
parseOp s   = I (intval s)

insn ::=
    Inp op      |
    Add op op   |
    Mul op op   |
    Div op op   |
    Mod op op   |
    Eql op op

isInp :: insn -> bool
isInp (Inp _) = True
isInp _       = False

|| extract integer literal operand 2 from insn 
viewOp2Lit :: insn -> maybe int
viewOp2Lit (Add _ (I n)) = Just n
viewOp2Lit (Mul _ (I n)) = Just n
viewOp2Lit (Div _ (I n)) = Just n
viewOp2Lit (Mod _ (I n)) = Just n
viewOp2Lit (Eql _ (I n)) = Just n
viewOp2Lit _             = Nothing

parseInsn :: string -> insn
parseInsn = words .> go
            where
              go ["inp", s]      = Inp (parseOp s)
              go ["add", sa, sb] = Add (parseOp sa) (parseOp sb)
              go ["mul", sa, sb] = Mul (parseOp sa) (parseOp sb)
              go ["div", sa, sb] = Div (parseOp sa) (parseOp sb)
              go ["mod", sa, sb] = Mod (parseOp sa) (parseOp sb)
              go ["eql", sa, sb] = Eql (parseOp sa) (parseOp sb)
              go _ = undef

blockInfo == m_map int [insn]

|| each block starts with an Inp insn; collect a list of insns at each
|| insn index of a block
genBlocks :: [insn] -> blockInfo
genBlocks
    = foldl go (0, m_empty) .> snd
      where
        go (i, m) ins
            = (i' + 1, m_insertWith cmpint (converse (++)) i' [ins] m)
              where
                i' = if' (isInp ins) 0 i

|| extract the varying op2 int components from the insns at index i in the blocks
extractVaryings :: blockInfo -> int -> [int]
extractVaryings m i
    = m_findWithDefault cmpint [] i m |> map viewOp2Lit |> catMaybes

|| The value of z through the 14 blocks forms a LIFO stack of base-26 numbers,
|| which is pushed to when the DIV z I immediate is 1, and popped from when it
|| is 26
solve :: bool -> [int] -> [int] -> [int] -> int
solve part1 xs ys zs
    = go [] v vs
      where
        v  = v_rep (#xs) undef
        vs = enumerate $ zip3 xs ys zs

        addDigit n d = n * 10 + d

        go stk vr ((i, (x, y, z)) : vs)
            = go ((i, y) : stk) vr  vs, if z == 1
            = go stk'           vr1 vs, if part1
            = go stk'           vr2 vs, otherwise
              where
                (j, o) : stk' = stk

                delta = x + o

                vr1   = vr // [(j, 9), (i, 9 + delta)], if delta < 0
                      = vr // [(j, 9 - delta), (i, 9)], otherwise

                vr2   = vr // [(j, 1), (i, 1 + delta)], if delta > 0
                      = vr // [(j, 1 - delta), (i, 1)], otherwise

        go _ vr _ = v_toList vr |> foldl addDigit 0

readInsns :: string -> io [insn]
readInsns fn = readFile fn >>= lines .> map parseInsn .> io_pure

main :: io ()
main = readInsns "../inputs/day24.txt" >>= genBlocks .> go
       where
         go blocks
             = io_mapM_ putStrLn [part1, part2]
               where
                 xs    = extractVaryings blocks 5
                 ys    = extractVaryings blocks 15
                 zs    = extractVaryings blocks 4
                 part1 = (++) "part 1: " . showint $ solve True  xs ys zs
                 part2 = (++) "part 2: " . showint $ solve False xs ys zs
