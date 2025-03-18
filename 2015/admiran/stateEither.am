|| stateEither.m -- functor / applicative / monad / alternative for combination of state and either


%export +

%import <either>


stateEither * ** *** == (* -> (either ** ***, *))

|| running
ste_runState :: stateEither * ** *** -> * -> (either ** ***, *)
ste_runState ma s = ma s

ste_evalState :: stateEither * ** *** -> * -> either ** ***
ste_evalState ma s = fst (ste_runState ma s)

ste_execState :: stateEither * ** *** -> * -> *
ste_execState ma s = snd (ste_runState ma s)


|| state access
ste_get :: stateEither * ** *
ste_get s = (Right s, s)

ste_put :: * -> stateEither * ** ()
ste_put s' = const (Right (), s')

ste_modify :: (* -> *) -> stateEither * ** ()
ste_modify f = go where go s = (Right (), f s)


|| monad interface
ste_bind :: stateEither * ** *** -> (*** -> stateEither * ** ****) -> stateEither * ** ****
ste_bind ma f
    = go
      where
        go s
            = case ste_runState ma s of
                (ra, s') -> case ra of
                              Left  x -> (Left x, s')
                              Right a -> ste_runState (f a) s'

ste_join :: stateEither * ** (stateEither * ** ***) -> stateEither * ** ***
ste_join ma
    = go
      where
        go s
            = case ste_runState ma s of
                (ra, s') -> case ra of
                              Left  x -> (Left x, s')
                              Right a -> ste_runState a s'

ste_pure :: *** -> stateEither * ** ***
ste_pure a = go where go s = (Right a, s)

ste_either :: either ** *** -> stateEither * ** ***
ste_either ma = go where go s = (ma, s)

ste_excpt :: ** -> stateEither * ** ***
ste_excpt x = go where go s = (Left x, s)

ste_kbind :: (*** -> stateEither * ** ****) -> (**** -> stateEither * ** *****) -> *** -> stateEither * ** *****
ste_kbind f g = go where go x = f x $ste_bind g

ste_mapM :: (*** -> stateEither * ** ****) -> [***] -> stateEither * ** [****]
ste_mapM f = foldr (ste_liftA2 (:) . f) (ste_pure [])

ste_mapM_ :: (*** -> stateEither * ** ****) -> [***] -> stateEither * ** ()
ste_mapM_ f = foldr (ste_right . f) (ste_pure ())

ste_foldM :: (**** -> *** -> stateEither * ** ****) -> **** -> [***] -> stateEither * ** ****
ste_foldM f z0 xs
    = foldr c ste_pure xs z0
      where
        c x k z = f z x $ste_bind k


|| functor interface
ste_fmap :: (*** -> ****) -> stateEither * ** *** -> stateEither * ** ****
ste_fmap f ma = ma $ste_bind (ste_pure . f)


|| applicative interface
ste_apply :: stateEither * ** (*** -> ****) -> stateEither * ** *** -> stateEither * ** ****
ste_apply mf ma = mf $ste_bind ($ste_fmap ma)

ste_liftA2 :: (*** -> **** -> *****) -> stateEither * ** *** -> stateEither * ** **** -> stateEither * ** *****
ste_liftA2 f ma mb = ste_fmap f ma $ste_apply mb

ste_liftA3 :: (*** -> **** -> ***** -> ******) -> stateEither * ** *** -> stateEither * ** **** -> stateEither * ** ***** -> stateEither * ** ******
ste_liftA3 f ma mb mc = ste_liftA2 f ma mb $ste_apply mc

ste_left :: stateEither * ** *** -> stateEither * ** **** -> stateEither * ** ***
ste_left ma mb = ste_liftA2 const ma mb

ste_right :: stateEither * ** *** -> stateEither * ** **** -> stateEither * ** ****
ste_right ma mb = ste_liftA2 (converse const) ma mb


|| lift multiple independent state actions and bind them to an N-ary function
ste_bind2 :: stateEither * ** *** -> stateEither  * ** **** -> (*** -> **** -> stateEither * ** *****) -> stateEither * ** *****
ste_bind2 ma mb f = ste_join (ste_liftA2 f ma mb)

ste_bind3 :: stateEither * ** *** -> stateEither * ** **** -> stateEither * ** ***** -> (*** -> **** -> ***** -> stateEither * ** ******) ->
             stateEither * ** ******
ste_bind3 ma mb mc f = ste_join (ste_liftA3 f ma mb mc)


|| alternative interface
|| note: if ma fails, then mb is run with the state returned from ma, instead of rewinding it to the original state
ste_alt :: stateEither * ** *** -> stateEither * ** *** -> stateEither * ** ***
ste_alt ma mb
    = go
      where
        go s
            = case ste_runState ma s of
                (ra, s') -> case ra of
                              Left  x -> ste_runState mb s'
                              Right a -> (Right a, s')

ste_some :: stateEither * ** *** -> stateEither * ** [***]
ste_some ma = ste_liftA2 (:) ma (ste_many ma)

ste_many :: stateEither * ** *** -> stateEither * ** [***]
ste_many ma = ste_some ma $ste_alt ste_pure []
