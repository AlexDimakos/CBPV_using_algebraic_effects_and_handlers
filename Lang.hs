{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}


module Lang where
import Data.Functor.Identity (Identity(..))


import Free

data Lang
  = Var String
  | Stringl String
  | Let String Lang Lang
  | Truel
  | Falsel
  | If Lang Lang Lang
  | Print Lang Lang
  | Lambda String Lang
  | App Lang Lang
  | Inl Lang
  | Inr Lang
  | Pm Lang Lang Lang


data Operation k = forall s. Show s => PrintOp s k

instance Show k => Show (Operation k) where
    show (PrintOp s k) = "PrintOp " ++ show s ++ " " ++  show k

hPrint :: Functor g => Handler_ Operation a String g (a, String)
hPrint = Handler_
  { ret_ = \x s -> pure (x, s)
  , hdlr_ = \x s -> case x of
      PrintOp s' k -> k (s ++ " "  ++ (show s')) }

instance Functor Operation where
  fmap f (PrintOp s k) = PrintOp s (f k)



printf :: (Operation < f, Show s) => s -> Free f ()
printf s = Op (inj (PrintOp s (Pure ())))
