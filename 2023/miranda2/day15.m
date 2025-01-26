|| day15.m -- Lens Library


%export day15

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <map>
%import <mirandaExtensions>

label == string
lens  == (label, int)

|| custom ordI for lens, comparing only the label
cmplens :: ordI lens
cmplens = cmplabel $on fst

box   == [lens]
boxes == m_map int box

hash :: string -> int
hash
    = foldl go 0
      where
        go s c = ((s + code c) * 17) $mod 256

removeLens :: label -> boxes -> boxes
removeLens lbl m
    = m_insert cmpint idx box' m
      where
        idx  = hash lbl
        box  = m_findWithDefault cmpint [] idx m
        box' = delete cmplens (lbl, 0) box

replaceLens :: lens -> box -> box
replaceLens x
    = go
      where
        go [] = [x]
        go (y : ys)
            = x : ys,    if _eq cmplens x y
            = y : go ys, otherwise

insertLens :: lens -> boxes -> boxes
insertLens (lbl, fl) m
    = m_insert cmpint idx box' m
      where
        idx  = hash lbl
        box  = m_findWithDefault cmpint [] idx m
        box' = replaceLens (lbl, fl) box

process :: boxes -> string -> boxes
process m cmd
    = removeLens lbl m,              if op ==. '-'
    = insertLens (lbl, intval fl) m, otherwise
      where
        (lbl, rest) = span letter cmd
        (op : fl)   = rest

initialize :: [string] -> boxes
initialize = foldl process m_empty

focusingPower :: boxes -> int
focusingPower
    = sum . map boxPower . m_toList
      where
        boxPower (i, xs) = (i + 1) * sum [(j + 1) * fl | (j, (lbl, fl)) <- enumerate xs]

readSequence :: string -> io [string]
readSequence fn = (split ',' . concat . lines) <$>. readFile fn

day15 :: io ()
day15
    = readSequence "../inputs/day15.txt" >>=. go
      where
        go sequence
            = io_mapM_ putStrLn [part1, part2]
              where
                part1 = "part 1: " ++ sequence |> map hash |> sum |> showint
                part2 = "part 2: " ++ sequence |> initialize |> focusingPower |> showint
