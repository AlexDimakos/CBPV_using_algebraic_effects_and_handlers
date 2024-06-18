{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}

module Thunk where

import Free
import Unsafe.Coerce


-- Thunks

-- Call by value
newtype CBVal a = CBVal { unCBVal :: a }

thunkCBVal :: Functor f => Free f a -> Free f (CBVal a)
thunkCBVal m = fmap CBVal m

forceCBVal :: CBVal a -> Free f a
forceCBVal (CBVal a) = Pure a


instance Show a => Show (CBVal a) where
  show :: Show a => CBVal a -> String
  show (CBVal a) = "CBVal " ++ show a



-- Call by name
newtype CBName f a = CBName (Free f a)

thunkCBName :: Free f a -> Free f (CBName f a)
thunkCBName m = Pure (CBName m)

forceCBName :: CBName f a -> Free f a
forceCBName (CBName m) = m


-- Call by need
newtype CBNeed f a = CBNeed (Int, Free f a)

data Pack = forall v. Pack (Maybe v)

thunkCBNeed :: (Functor f, (State [Pack]) < f) => Free f a -> Free f (CBNeed f a)
thunkCBNeed m = do
  s <- get
  put (s ++ [Pack Nothing])
  return (CBNeed (length s, m))

forceCBNeed :: (Functor f, State [Pack] < f) => CBNeed f a -> Free f a
forceCBNeed (CBNeed (n, m)) = do
  s <- get
  case s !! n of
    Pack (Just v) -> return (unsafeCoerce v)
    Pack Nothing  -> do
      v <- m
      put (updateList n (Pack (Just v)) s)
      return v

updateList :: Int -> a -> [a] -> [a]
updateList idx newVal (x:xs)
  | idx == 0  = newVal : xs
  | otherwise = x : updateList (idx-1) newVal xs
updateList _ _ [] = []

lawExample :: Free (State [Pack]) Int
lawExample = thunkCBName (Pure 1) >>= forceCBName

lawExample2 :: Free (State [Pack] + End) Int
lawExample2 = thunkCBNeed (Pure 1) >>= forceCBNeed

-- Lam
newtype Fun f t1 t2 = Fun { app :: t1 -> Free f t2 }


lam :: (a -> Free f b) -> Free f (Fun f a b)
lam f = Pure (Fun f)

-- App

apply :: Fun f a b -> a -> Free f b
apply = app
