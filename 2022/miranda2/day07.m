%export day07

%import <io> (>>=.)/io_bind (<$>.)/io_fmap
%import <map>
%import <maybe>
%import <mirandaExtensions>
%import <parser>
%import <state>


|| file system definitions

ident == string
dir   == m_map ident entry
entry ::= File int | Dir dir (maybe int)

isDirEntry :: entry -> bool
isDirEntry (Dir d ms) = True
isDirEntry e          = False

getDir :: entry -> dir
getDir (Dir d ms) = d
getDir e          = error "getDir on non-dir entry"

entrySize :: entry -> int
entrySize (File n)         = n
entrySize (Dir d (Just n)) = n
entrySize e                = error "entry size missing"

emptyFileSystem :: dir
emptyFileSystem = m_singleton "/" (Dir m_empty Nothing)


|| parsing commands

cmd      ::= CD string | LS [lsResult]
lsResult ::= Dresult string | Fresult int string

p_prompt :: parser char
p_prompt = p_char '$' $p_left p_spaces

p_ident :: parser string
p_ident
    = p_some (p_satisfy identChar) $p_left p_spaces
      where
        identChar c = letter c \/ digit c \/ c ==. '.' \/ c ==. '/'

p_size :: parser int
p_size = p_posint $p_left p_spaces

p_file :: parser lsResult
p_file = p_liftA2 Fresult p_size p_ident

p_dir :: parser lsResult
p_dir = Dresult $p_fmap (p_string "dir" $p_right p_spaces $p_right p_ident)

p_lsResult :: parser lsResult
p_lsResult = p_file $p_alt p_dir

p_ls :: parser cmd
p_ls = LS $p_fmap (p_prompt $p_right p_string "ls" $p_right p_spaces $p_right p_some p_lsResult)

p_cd :: parser cmd
p_cd = CD $p_fmap (p_prompt $p_right p_string "cd" $p_right p_spaces $p_right p_ident)

p_cmd :: parser cmd
p_cmd = p_cd $p_alt p_ls

p_cmds :: parser [cmd]
p_cmds = p_some p_cmd $p_left p_end

|| building the file system from the commands

runCommand :: dir -> state [cmd] dir

|| finished with the commands; return current dir
runCommand d []             = (d, [])

|| finished with the current dir, return it 
runCommand d (CD ".." : cs) = (d, cs)

|| process commands in a subdirectory, then continue
|| in the current directory
runCommand d (CD n : cs)
    = error ("can't cd to " ++ n), if isNothing me \/ ~isDirEntry e
    = runCommand d' cs',           otherwise
      where
        me         = m_lookup cmpident n d
        e          = fromJust me
        (Dir sd _) = e
        (sd', cs') = runCommand sd cs                           || run commands in the subdirectory
        sdSize     = (sum . map entrySize . m_elems) sd'        || compute the size of the subdirectory
        d'         = m_insert cmpident n (Dir sd' (Just sdSize)) d       || insert computed size and updated subdirectory

|| get entry info about the current directory
runCommand d (LS es : cs)
    = runCommand (foldl addEntry d es) cs
      where
        addEntry d (Fresult n s) = m_insert cmpident s (File n) d
        addEntry d (Dresult s)   = m_insert cmpident s (Dir m_empty Nothing) d

|| build the file system by running commands on an empty one
buildFileSystem :: [cmd] -> dir
buildFileSystem = fst . runCommand emptyFileSystem 

|| find all entries (recursively) in a directory that match a predicate
findEntries :: (entry -> bool) -> dir -> [entry]
findEntries p d
    = pfs ++ pds
      where
        (ds, fs) = partition isDirEntry (m_elems d)
        pfs      = filter p fs
        pds      = filter p ds ++ concatMap (findEntries p . getDir) ds

|| is an entry a directory with size at most 100000?
pred1 :: entry -> bool
pred1 (Dir d (Just n)) = n <= 100000
pred1 e                = False

|| is an entry a directory with size at least m?
pred2 :: int -> entry -> bool
pred2 m (Dir d (Just n)) = n >= m
pred2 m e                = False

readInput :: string -> io [cmd]
readInput fn
    = go <$>. readFile fn
      where
        go input
            = error (p_error ps), if isNothing pr
            = fromJust pr,        otherwise
              where
                (pr, ps) = parse (p_some p_cmd) input

day07 :: io ()
day07
    = readInput "../inputs/day07.txt" >>=. go
      where
        go cmds
            = io_mapM_ putStrLn [part1, part2]
              where
                fs    = buildFileSystem cmds
                used  = entrySize (fromJust (m_lookup cmpident "/" fs))
                free  = 70000000 - used
                need  = 30000000 - free
                part1 = (++) "part 1: " . showint . sum . map entrySize . findEntries pred1 $ fs
                part2 = (++) "part 2: " . showint . min cmpint . map entrySize . findEntries (pred2 need) $ fs
