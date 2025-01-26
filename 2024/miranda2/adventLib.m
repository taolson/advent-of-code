|| advent library


%export + <io> <mirandaExtensions> (>>=) (<$>) (<*>) (<<) (>>)

%import <io>
%import <mirandaExtensions>
%import <state>                 (>>=)/st_bind (<$>)/st_fmap (<*>)/st_apply (<<)/st_left (>>)/st_right


format :: (int, string) -> string
format (i, s) = "part " ++ showint i ++ ": " ++ s

output :: [string] -> io ()
output rs = zip2 [1 ..] rs |> io_mapM_ (format .> putStrLn)
