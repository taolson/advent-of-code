|| ring.m -- ring for day09


%import <mirandaExtensions>

ring * ::= Ring [*] ! * ! [*] !

r_right :: ring * -> ring *
r_right (Ring [] n [])       = Ring [] n []
r_right (Ring ls n [])       = Ring [] (hd ls') (tl ls') where ls' = reverse (n : ls)
r_right (Ring ls n (x : xs)) = Ring (n : ls) x xs

r_left :: ring * -> ring *
r_left (Ring [] n [])       = Ring [] n []
r_left (Ring [] n rs)       = Ring (tl rs') (hd rs') [] where rs' = reverse(n : rs)
r_left (Ring (x : xs) n rs) = Ring xs x (n : rs)

r_current :: ring * -> *
r_current (Ring _ n _) = n

r_add :: * -> ring * -> ring *
r_add x (Ring ls n rs) = Ring (n : ls) x rs

r_delete :: ring * -> ring *
r_delete (Ring ls _ [])       = Ring [] (hd ls') (tl ls') where ls' = reverse ls
r_delete (Ring ls _ (x : xs)) = Ring ls x xs

r_modify :: (* -> *) -> ring * -> ring *
r_modify f (Ring ls n rs) = Ring ls (f n) rs

|| functor interface
r_fmap :: (* -> **) -> ring * -> ring **
r_fmap f (Ring ls n rs) = Ring (map f ls) (f n) (map f rs)

|| fold interface
r_foldl :: (* -> ** -> *) -> * -> ring ** -> *
r_foldl f z (Ring ls n rs) = foldl f (f (foldl f z ls) n) rs
