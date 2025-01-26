|| day25.m


%export day25

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <maybe>
%import <mirandaExtensions>
%import <parser> (<$>)/p_fmap (<<)/p_left (>>)/p_right (<|>)/p_alt
%import <vector> -(!!)


tape == ([int], int, [int])     || infinite tape of 1/0

tinit :: tape
tinit = ([], 0, [])

tread :: tape -> int
tread (_, c, _) = c

twrite :: int -> tape -> tape
twrite c (l, _, r) = (l, c, r)

tsum :: tape -> int
tsum (l, c, r) = sum l + c + sum r

movel, mover :: tape -> tape
movel ([], c, r)     = ([], 0, c : r)
movel (l : ls, c, r) = (ls, l, c : r)

mover (l, c, [])     = (c : l, 0, [])
mover (l, c, r : rs) = (c : l, r, rs)


transition == (int, (tape -> tape), int)        || value to write, move, next state idx
state      == vector transition                 || a 2-element vector of transitions; first for read of 0, second for read of 1
machine    == vector state                      || state machine
stateIdx   == int

toStIdx :: char -> int
toStIdx c = code c - code 'A'

                           
p_stateIdx :: parser stateIdx
p_stateIdx = toStIdx <$> p_letter

p_mov :: parser (tape -> tape)
p_mov
    = mkMove <$> p_word
      where
        mkMove "left"  = movel
        mkMove "right" = mover
        mkMove _       = error "parse error in move direction"

p_initSt :: parser stateIdx
p_initSt = p_string "Begin in state " >> p_stateIdx << p_char '.' << p_spaces

p_steps :: parser int
p_steps = p_string "Perform a diagnostic checksum after " >> p_int << p_string " steps." << p_spaces

p_transition :: parser (int, transition)
p_transition
    = p_liftA4 mkTransition
          (p_string "If the current value is " >> p_int << p_char ':' << p_spaces)
          (p_string "- Write the value " >> p_int << p_char '.' << p_spaces)
          (p_string "- Move one slot to the " >> p_mov << p_char '.' << p_spaces)
          (p_string "- Continue with state " >> p_stateIdx << p_char '.' << p_spaces)
      where
        mkTransition r w mov si = (r, (w, mov, si))

p_state :: parser (stateIdx, state)
p_state
    = p_liftA2 mkState
          (p_string "In state " >> p_stateIdx << p_char ':' << p_spaces)
          (p_some p_transition)
      where
        mkState si ts = (si, v_rep (#ts) undef // ts)

p_machine :: parser machine
p_machine
    = mkMachine <$> p_some p_state
      where
        mkMachine ss = v_rep (#ss) undef // ss

p_input :: parser (machine, stateIdx, int)
p_input
    = p_liftA3 mkInput p_initSt p_steps p_machine
      where
        mkInput si steps m = (m, si, steps)
    
v !! i = v_unsafeIndex v i

run :: machine -> tape -> int -> int -> tape
run m t si 0 = t
run m t si n
    = case tread t of
        r -> case m !! si !! r of
               (w, mov, si') -> case mov (twrite w t) of
                                  t' -> run m t' si' (n - 1)

readInput :: string -> io (machine, stateIdx, int)
readInput fn
    = go <$>. parse p_input <$>. readFile fn
      where
        go (mr, ps) = fromMaybe (error (p_error ps)) mr

day25 :: io ()
day25
    = readInput "../inputs/day25.input" >>=. go
      where
        go (m, si, steps)
            = io_mapM_ putStrLn [part1, tsize t, tpos t]
              where
                t               = run m tinit si steps
                part1           = (++) "part 1: " . showint . tsum $ t
                tsize (l, c, r) = (++) "final tape size: " . showint $ #l + 1 + #r
                tpos  (l, c, r) = (++) "final tape position: " . showint $ #l + 1
