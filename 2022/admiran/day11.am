%export day11

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <parser>
%import <dequeue>
%import <either>
%import <lens>
%import <map>
%import <maybe>
%import <mirandaExtensions>


item == int

monkey
    == ( int              || monkey intber
       , dequeue item     || items being held
       , (int -> int)     || operation (old -> new)
       , int              || test value
       , (int -> int)     || test on value, returns which monkey to throw to
       , int              || total inspections
       )

showmonkey = undef
cmpmonkey  = undef

|| lenses for monkey
mky_idx         = lensTup6_0
mky_items       = lensTup6_1
mky_op          = lensTup6_2
mky_tstVal      = lensTup6_3
mky_tst         = lensTup6_4
mky_inspections = lensTup6_5

makeMonkey :: int -> [int] -> char -> (either string int) -> int -> int -> int -> monkey
makeMonkey idx items opChar opVal divBy trueIdx falseIdx
    = (idx, dq_fromList items, op, divBy, computeTarget, 0)
      where
        op x = (x $binOp fromEither x opVal)

        binOp
            = (+), if opChar ==. '+'
            = (-), if opChar ==. '-'
            = (*), if opChar ==. '*'
            = error ("unexpected operation: " ++ [opChar]), otherwise

        computeTarget x
            = trueIdx, if divisible
            = falseIdx, otherwise
              where
                divisible = x $mod divBy == 0


|| parsing
p_liftA7 f ma mb mc md me mf mg = (((p_liftA4 f ma mb mc md) $p_apply me) $p_apply mf) $p_apply mg

p_test :: string -> parser int
p_test s = p_string "If " $p_right p_string s $p_right p_string ": throw to monkey " $p_right p_posint

p_operand :: parser (either string int)
p_operand = p_fmap Right p_posint $p_alt
            p_fmap Left  (p_many p_letter)

p_monkey :: parser monkey
p_monkey = p_liftA7 makeMonkey
               (p_string "Monkey " $p_right p_posint $p_left p_char ':' $p_left p_spaces)
               (p_string "Starting items: " $p_right  p_manySepBy (p_string ", ") p_posint $p_left p_spaces)
               (p_string "Operation: new = old " $p_right p_any $p_left p_spaces)
               (p_operand $p_left p_spaces)
               (p_string "Test: divisible by " $p_right p_posint $p_left p_spaces)
               (p_test "true" $p_left p_spaces)
               (p_test "false" $p_left p_spaces)


|| running
mkyMap == m_map int monkey

turn :: (int -> int) -> mkyMap -> int -> mkyMap
turn reducer mkys idx
    = m_insert cmpint idx mky' (foldl throwTo mkys (zip2 dest worry))
      where
        mky   = fromJust (m_lookup cmpint idx mkys)
        mky'  = (set mky_items dq_empty . over mky_inspections (+ #items)) mky
        items = (dq_toList . view mky_items) mky
        worry = map (reducer . view mky_op mky) items
        dest  = map (view mky_tst mky) worry

        throwTo mkys (i, x)
            = m_adjust cmpint addItem i mkys
              where
                addItem = over mky_items (dq_addR x)

round :: (int -> int) -> mkyMap -> mkyMap
round reducer mkys
    = foldl (turn reducer) mkys (m_keys mkys)

readMonkeys :: string -> io (m_map int monkey)
readMonkeys fn
    = go <$>. parse (p_many p_monkey $p_left p_end) <$>. readFile fn
      where
        go (pr, ps)
            = error (p_error ps),              if isNothing pr
            = foldl ins m_empty (fromJust pr), otherwise
              where
                ins m mky = m_insert cmpint (view mky_idx mky) mky m

day11 :: io ()
day11
    = readMonkeys "../inputs/day11.txt" >>=. go
      where
        go monkeys
            = io_mapM_ putStrLn [part1, part2]
              where
                reducer1 = ($div 3)
                reducer2 = (converse mod . product . map (view mky_tstVal) . m_elems) monkeys
                process  = showint . product . take 2 . sortBy (descending cmpint id) . map (view mky_inspections) . m_elems
                part1    = (++) "part 1: " . process . (! 20)    . iterate (round reducer1) $ monkeys
                part2    = (++) "part 2: " . process . (! 10000) . iterate (round reducer2) $ monkeys
