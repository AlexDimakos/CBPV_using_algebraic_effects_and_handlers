{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module CBNeed_Translation where

import Thunk
import Lang
import Free
import Unsafe.Coerce

data Value f
  = VTrue
  | VFalse
  | VFun (Free f (Fun f (CBNeed f (Value f)) (Value f)))
  | VInl (CBNeed f (Value f))
  | VInr (CBNeed f (Value f))
  | VString String

instance Show (Value f) where
  show VTrue = "True"
  show VFalse = "False"
  show (VFun _) = "Function"
  show (VInl _) = "Inl"
  show (VInr _) = "Inr"
  show (VString s) = s

instance Show Pack where
  show (Pack (Just v)) = "Pack " ++ show (unsafeCoerce v :: (Value f))
  show (Pack Nothing)  = "Pack Nothing"


instance Show s => Show (Free f s) where
  show (Pure x) = "Pure " ++ show x
  show (Op x) = "Op _" 



data Env f s = Env { cells :: [(String, CBNeed f s)] }

lookupEnv :: String -> Env f s -> Maybe (CBNeed f s)
lookupEnv x (Env env) = lookup x env

denoteCBNeed :: (Functor f, Operation < f, State [Pack] < f) => Env f (Value f) -> Lang -> Free f (Value f)
denoteCBNeed env (Var x) = case (lookupEnv x env) of
  Just v -> forceCBNeed v
  Nothing -> undefined
denoteCBNeed env (Let x e1 e2) = do
  v <- thunkCBNeed (denoteCBNeed env e1)
  denoteCBNeed (extendEnv x v env) e2
denoteCBNeed _ Truel = Pure VTrue
denoteCBNeed _ Falsel =  Pure VFalse
denoteCBNeed _ (Stringl s) = Pure (VString s)
denoteCBNeed env (Lambda x body) =
  Pure $ VFun (lam (\v -> denoteCBNeed (extendEnv x v env) body))
denoteCBNeed env (App e1 e2) = do
  v1 <- thunkCBNeed (denoteCBNeed env e1)
  v2 <- denoteCBNeed env e2
  case v2 of
    (VFun (Pure f)) -> apply f v1
    _ -> undefined
denoteCBNeed env (Print e1 e2) = do
  v1 <- denoteCBNeed env e1
  printf v1
  denoteCBNeed env e2

denoteCBNeed env (If e1 e2 e3) = do
  v1 <- thunkCBNeed (denoteCBNeed env e1)
  case (denoteCBNeed (extendEnv "z" v1 env) (Var "z")) of
    Pure (VTrue) -> denoteCBNeed (extendEnv "z" v1 env) e2
    Pure (VFalse) -> denoteCBNeed (extendEnv "z" v1 env) e3
    _ -> undefined
denoteCBNeed env (Inl e) = do
  v <- thunkCBNeed (denoteCBNeed env e)
  return (VInl v)
denoteCBNeed env (Inr e) = do
  v <- thunkCBNeed (denoteCBNeed env e)
  return (VInr v)
denoteCBNeed env (Pm e (Inl (Lambda x body1)) (Inr (Lambda y body2))) = do
  v <- denoteCBNeed env e
  case v of
    VInl val -> denoteCBNeed (extendEnv x val env) body1
    VInr val -> denoteCBNeed (extendEnv y val env) body2
    _ -> undefined
denoteCBNeed _ _ = undefined

emptyEnvCBNeed :: Env (State [Pack] + Operation + End) (Value f)
emptyEnvCBNeed = Env []

extendEnv :: String -> CBNeed f s -> Env f s -> Env f s
extendEnv x val (Env env) = Env ((x, val) : env)

testcbv ::Free (State [Pack] + Operation) (CBVal Int)
testcbv = thunkCBVal (Op (L (Get (\s -> Pure 3))))

testcbn ::Free (State [Pack] + Operation) (CBName (State [Pack] + Operation) Int)
testcbn = thunkCBName (Op (L (Get (\s -> Pure 3))))


-- print "Hello1"
-- x = thunk (print "Hello2" >> return True)
-- print "Hello3"
-- print x
-- x
exampleCBPV :: Free ((State [Pack]) + Operation  + End) (Value f)
exampleCBPV = do 
  printf (VString "Hello1")
  v <- thunkCBNeed (do
    printf (VString "Hello2")
    return VTrue)
  printf (VString "Hello3")
  x <- forceCBNeed v
  printf x
  x' <- forceCBNeed v
  return VTrue

