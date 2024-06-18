{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module CBName_Translation where

import Thunk
import Lang
import Free  

data Value f 
  = VTrue 
  | VFalse 
  | VString String
  | VFun (Free f (Fun f (CBName f (Value f)) (Value f)))  
  | VInl (CBName f (Value f))
  | VInr (CBName f (Value f))

instance Show (Value f) where
  show VTrue = "True"
  show VFalse = "False"
  show (VFun _) = "Function"
  show (VInl _) = "Inl"
  show (VInr _) = "Inr"
  show (VString s) = s

data Env f s = Env { cells :: [(String, CBName f s)] }

lookupEnv :: String -> Env f s -> Maybe (CBName f s)
lookupEnv x (Env env) = lookup x env

denoteCBName :: (Functor f, Operation < f) => Env f (Value f) -> Lang -> Free f (Value f)
denoteCBName env (Var x) = case (lookupEnv x env) of
  Just v -> forceCBName v
  Nothing -> undefined
denoteCBName env (Let x e1 e2) = do
  v <- thunkCBName (denoteCBName env e1)
  denoteCBName (extendEnv x v env) e2
denoteCBName _ Truel = Pure VTrue
denoteCBName _ Falsel =  Pure VFalse
denoteCBName _ (Stringl s) = Pure (VString s)
denoteCBName env (Lambda x body) = 
  Pure $ VFun (lam (\v -> denoteCBName (extendEnv x v env) body))
denoteCBName env (App e1 e2) = do
  v1 <- thunkCBName (denoteCBName env e1)
  v2 <- denoteCBName env e2
  case v2 of
    (VFun (Pure f)) -> apply f v1
    _ -> undefined
denoteCBName env (If e1 e2 e3) = do
  v1 <- thunkCBName (denoteCBName env e1)
  case (denoteCBName (extendEnv "z" v1 env) (Var "z")) of
    Pure (VTrue) -> denoteCBName (extendEnv "z" v1 env) e2
    Pure (VFalse) -> denoteCBName (extendEnv "z" v1 env) e3
    _ -> undefined
denoteCBName env (Inl e) = do
  v <- thunkCBName (denoteCBName env e) 
  return (VInl v)
denoteCBName env (Inr e) = do
  v <- thunkCBName (denoteCBName env e)
  return (VInr v)
denoteCBName env (Pm e (Inl (Lambda x body1)) (Inr (Lambda y body2))) = do 
  v <- denoteCBName env e
  case v of
    VInl val -> denoteCBName (extendEnv x val env) body1
    VInr val -> denoteCBName (extendEnv y val env) body2
    _ -> undefined
denoteCBName env (Print e1 e2) = do
  v1 <- denoteCBName env e1
  printf v1
  denoteCBName env e2
denoteCBName _ _ = undefined

emptyEnvCBName :: Env (Operation + Operation) (Value f)
emptyEnvCBName = Env []

extendEnv :: String -> CBName f s -> Env f s -> Env f s
extendEnv x val (Env env) = Env ((x, val) : env)