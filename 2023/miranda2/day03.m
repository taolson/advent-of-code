|| day03.m -- Gear Ratios


%export day03

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import <parser> (<$>)/p_fmap (<<)/p_left (>>)/p_right (<|>)/p_alt
%import <set>


loc    == (int, int)

part   ::= Part int loc int       || part#, location, length

partNumber :: part -> int
partNumber (Part n lc ln) = n

partLoc :: part -> loc
partLoc (Part n lc ln) = lc

partLen :: part -> int
partLen (Part n lc ln) = ln


symbol ::= Sym char loc         || sym, location

symbolLoc :: symbol -> loc
symbolLoc (Sym c lc) = lc

isStar :: symbol -> bool
isStar (Sym '*' lc) = True
isStar s            = False

symLocs   == s_set loc          || set of locations for all the symbols
partLocs  == m_map loc [part]   || map from all of the locations adjacent to a part to the list of parts they are adjacent to
schematic == ([part], [symbol], partLocs, symLocs)


|| parse the schematic as a list of components, then put them together
component ::= Cpart part | Csym symbol

|| get the current line and col location of the parse
p_getLoc :: parser loc
p_getLoc (il, ic, cs) = (Just ((I# il), (I# ic)), (il, ic, cs))

p_part :: parser part
p_part
    = p_liftA3 mkPart p_getLoc p_posint p_getLoc
      where
        mkPart (r, c) n (_, c') = Part n (r, c) (c' - c)

p_sym :: parser symbol
p_sym
    = p_liftA2 mkSym p_getLoc (p_satisfy isSymbol)
      where
        isSymbol c = c ~=. '.' & ~digit c & ~isSpace c
        mkSym lc s = Sym s lc

p_noise :: parser [char]
p_noise = p_many (p_space <|> p_char '.')

p_component :: parser component
p_component =  p_noise >> ((Csym <$> p_sym) <|> (Cpart <$> p_part))

|| create the list of adjacent locations for a part
partAdjacencies :: part -> [loc]
partAdjacencies p
    = [(r', c') | r' <- [r - 1 .. r + 1];
                  c' <- [c - 1 .. c + sz];
                  r' ~= r \/ c' < c \/ c' > c + sz - 1]
      where
        (r, c) = partLoc p
        sz     = partLen p

adjacentToSymbol :: symLocs -> part -> bool
adjacentToSymbol sm
    = any isSymLoc . partAdjacencies
      where
        isSymLoc lc = s_member cmploc lc sm

adjacentParts :: partLocs -> symbol -> [part]
adjacentParts pm s = m_findWithDefault cmploc [] (symbolLoc s) pm

gearRatio :: partLocs -> symbol -> int
gearRatio pm s
    = 0,                              if #adj ~= 2
    = product . map partNumber $ adj, otherwise
      where
        adj = adjacentParts pm s

|| read all of the components, then build the schematic
readSchematic :: string -> io schematic
readSchematic fn
    = doParse <$>. readFile fn
      where
        doParse input
            = fromMaybef err mkSchematic mcs
              where
                (mcs, ps) = parse (p_some p_component) input
                err       = error (p_error ps)

        mkSchematic
            = foldl addComponent ([], [], m_empty, s_empty)
              where
                addComponent scm (Cpart p) = addPart p scm
                addComponent scm (Csym s)  = addSym s scm
                
                addPart p (ps, ss, pm, sm) = (p : ps, ss, (addPartLocs p pm), sm)
                addSym  s (ps, ss, pm, sm) = (ps, s : ss, pm, (addSymLoc s sm))

                addPartLocs p pm
                    = foldl ins pm (partAdjacencies p)
                      where
                        ins m lc = m_insertWith cmploc (++) lc [p] m

                addSymLoc s m = s_insert cmploc (symbolLoc s) m

day03 :: io ()
day03
    = readSchematic "../inputs/day03.txt" >>=. go
      where
        go (ps, ss, pm, sm)
            = io_mapM_ putStrLn [part1, part2]
              where
                part1 = ("part 1: " ++) . showint . sum . map partNumber . filter (adjacentToSymbol sm) $ ps
                part2 = ("part 2: " ++) . showint . sum . map (gearRatio pm) . filter isStar $ ss
