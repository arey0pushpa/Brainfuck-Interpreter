module MyStateMonad where

newtype State s a = State {
  state :: (s -> (s, a))
}

instance Monad (State s) where
  (State g) >>= f = State $ \s -> let (s', a)  = g s
                                      State f' = f a
                                  in f' s'
  return x = State $ \s -> (s, x)

get :: State s s
get = State $ \s -> (s,s)

put :: s -> State s () 
put v = State $ \_ -> (v, ())

modify :: (s -> s) -> State s ()
modify f = do
  s <- get
  put (f s)

runState :: State s a -> s -> (s, a)
runState (State f) s = f s 

