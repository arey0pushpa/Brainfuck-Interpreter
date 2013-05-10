module MyStateMonad where

newtype MyState s a = MyState {
  state :: (s -> (s, a))
}

instance Monad (MyState s) where
  (MyState g) >>= f = MyState $ \s -> let (s', a)  = g s
                                          MyState f' = f a
                                  in f' s'
  return x = MyState $ \s -> (s, x)

get :: MyState s s
get = MyState $ \s -> (s,s)

put :: s -> MyState s () 
put v = MyState $ \_ -> (v, ())

modify :: (s -> s) -> MyState s ()
modify f = do
  s <- get
  put (f s)

runState :: MyState s a -> s -> (s, a)
runState (MyState f) s = f s 

instance Functor (MyState a) where
  fmap f s = s >>= return . f
