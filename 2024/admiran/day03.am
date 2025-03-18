|| day03.m -- Mull It Over


%export day03

%import "adventLib"
%import <maybe>
%import <parser>        (>>=?)/p_bind (<$>?)/p_fmap (<<?)/p_left (>>?)/p_right (<|>?)/p_alt


insn ::= Mul int int | Do | Dont

isMul :: insn -> bool
isMul (Mul _ _) = True
isMul _         = False

program == [insn]

|| parse from lo to hi occurrences of p
p_range :: int -> int -> parser * -> parser [*]
p_range lo hi p
    = go 0
      where
        go n = p_next,                if n < lo || don't have required minimum, yet
             = p_pure [],             if n > hi || finished
             = p_next <|>? p_pure [], otherwise || have required amount; continue or terminate
               where
                 p_next = p $p_cons go (n + 1)

|| parse a number from 1 to 3 digits long
|| Note: this turned out to be overkill; the input didn't
|| contain any occurrences of numbers with > 3 digits.
|| However, i'll leave it as it doesn't alter the performance,
|| and may be useful in the future.
p_num :: parser int
p_num = intval <$>? p_range 1 3 p_digit

p_mul, p_dont, p_do, p_insn :: parser insn
p_mul  = p_liftA2 Mul  (p_string "mul(" >>? p_num) (p_comma >>? p_num) <<? p_char ')'
p_do   = p_pure   Do   <<? p_string "do()"
p_dont = p_pure   Dont <<? p_string "don't()"
p_insn = p_mul <|>? p_do <|>? p_dont

p_program :: parser program
p_program
    = (p_insn $p_cons p_program) <|>?   || try an insn
      (p_any >>? p_program)      <|>?   || or skip a character
      p_pure []                         || or end of input

parseProgram:: string -> program
parseProgram input
    = fromMaybe err mprog
        where
          (mprog, ps) = parse p_program input
          err         = error (p_error ps)

exec :: program -> int
exec
    = foldl go (Do, 0) .> snd
      where
        go (Do, n) (Mul a b) = (Do, n + a * b)
        go (_, n)  cmd       = (cmd, n), if ~isMul cmd
        go s       _         = s   

day03 :: io ()
day03
    = readFile "../inputs/day03.txt" >>= parseProgram .> go
      where
        go prog
            = output [part1, part2]
              where
                part1 = filter isMul prog |> exec |> showint
                part2 = exec prog |> showint
