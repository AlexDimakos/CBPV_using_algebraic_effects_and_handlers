{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Free where  

data Free f a
  = Pure a
  | Op (f (Free f a))
  
data State s k
  = Put s k
  | Get (s -> k)
  deriving Functor

preinc :: Free (State Int) Int
preinc = Op (Get (\s -> Op (Put (s + 1) (Pure s))))

infixr 6 +
data (f + g) a
  = L (f a)
  | R (g a)
  deriving Functor

data Err k = Err String
    deriving Functor

data End k -- No constructors!
  deriving Functor

incerr :: Free (Err + State Int + End) a
incerr =  Op (R (L (Get (\s ->
            Op (R (L (Put (s + 1)
              (Op (L (Err "foo"))))))))))

    
incerr' :: Free (Err + State Int + End) a
incerr' = do (s :: Int) <- get; put (s + 1); err "foo"


class f < g where
  inj :: f k -> g k

instance {-# OVERLAPPING #-} f  < f where inj = id
instance {-# OVERLAPPING #-} f < (f + g) where inj = L
instance f < h => f < (g + h) where inj = R . inj

get :: State s < f => Free f s
get = Op (inj (Get Pure))

put  :: State s < f => s -> Free f ()
put s = Op (inj (Put s (Pure ())))

err :: Err < f => String -> Free f a
err msg = Op (inj (Err msg))

fold :: Functor f => (a -> b) -> (f b -> b) -> Free f a -> b
fold gen _   (Pure x) = gen x
fold gen alg (Op f)   = alg (fmap (fold gen alg) f)

instance Functor f => Monad (Free f) where
  m >>= k = fold k Op m 

instance Functor f => Functor (Free f) where
  fmap f = fold (pure . f) Op

instance Functor f => Applicative (Free f) where
  pure = Pure
  f <*> m = fold (flip fmap m) Op f


data Handler f a f' b
  = Handler { ret  :: a -> Free f' b
            , hdlr :: f (Free f' b) -> Free f' b }

handle :: (Functor f, Functor f')
       => Handler f a f' b -> Free (f + f') a -> Free f' b
handle h = fold
  (ret h)
  (\x -> case x of
     L y -> hdlr h y
     R y -> Op y)

hErr :: Functor f' => Handler Err a f' (Either String a)
hErr = Handler 
  { ret = pure . Right
  , hdlr = \x -> case x of Err s -> pure (Left s) }

data Handler_ f a p f' b
  = Handler_ { ret_  :: a -> (p -> Free f' b)
             , hdlr_ :: f (p -> Free f' b) -> (p -> Free f' b) }

handle_ :: (Functor f, Functor f')
        => Handler_ f a p f' b -> Free (f + f') a
        -> p -> Free f' b
handle_ h = fold
  (ret_ h)
  (\x -> case x of
     L x -> hdlr_ h x
     R x -> \p -> Op (fmap (\m -> m p) x))

hState :: Functor g => Handler_ (State s) a s g (a, s)
hState = Handler_
  { ret_ = \x s -> pure (x, s)
  , hdlr_ = \x s -> case x of
      Put s' k -> k s'
      Get k -> k s s }

hState' :: Functor g => Handler_ (State s) a [s] g (a, [s])
hState' = Handler_
  { ret_ = \x ss -> pure (x, ss)
  , hdlr_ = \x ss -> case x of
      Put s' k -> k (s':ss)
      Get k -> k (head ss) ss }

un :: Free End a -> a
un (Pure x) = x
