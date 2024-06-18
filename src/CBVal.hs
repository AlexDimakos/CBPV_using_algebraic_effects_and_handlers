{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module CBVal where

import Thunk
import Lang
import Free  

data Value f 
  = VTrue 
  | VFalse 
  | VString String
  | VFun (Free f (Fun f (CBVal (Value f)) (Value f)))  
  | VInl (CBVal (Value f))
  | VInr (CBVal (Value f))

instance Show (Value f) where
  show VTrue = "True"
  show VFalse = "False"
  show (VFun _) = "Function"
  show (VInl _) = "Inl"
  show (VInr _) = "Inr"
  show (VString s) = s


data Env s = Env { cells :: [(String, CBVal s)] }

lookupEnv :: String -> Env s -> Maybe (CBVal s)
lookupEnv x (Env env) = lookup x env

denoteCBVal :: (Functor f, Operation < f) => Env (Value f) -> Lang -> Free f (Value f)
denoteCBVal env (Var x) = case (lookupEnv x env) of
  Just v -> forceCBVal v
  Nothing -> undefined
denoteCBVal env (Let x e1 e2) = do
  v <- thunkCBVal (denoteCBVal env e1)
  denoteCBVal (extendEnv x v env) e2
denoteCBVal _ Truel = Pure VTrue
denoteCBVal _ Falsel =  Pure VFalse
denoteCBVal _ (Stringl s) = Pure (VString s)
denoteCBVal env (Lambda x body) = 
  Pure $ VFun (lam (\v -> denoteCBVal (extendEnv x v env) body))
denoteCBVal env (App e1 e2) = do
  v1 <- thunkCBVal (denoteCBVal env e1)
  v2 <- denoteCBVal env e2
  case v2 of
    (VFun (Pure f)) -> apply f v1
    _ -> undefined
denoteCBVal env (If e1 e2 e3) = do
  v1 <- thunkCBVal (denoteCBVal env e1)
  case (denoteCBVal (extendEnv "z" v1 env) (Var "z")) of
    Pure (VTrue) -> denoteCBVal (extendEnv "z" v1 env) e2
    Pure (VFalse) -> denoteCBVal (extendEnv "z" v1 env) e3
    _ -> undefined
denoteCBVal env (Inl e) = do
  v <- thunkCBVal (denoteCBVal env e) 
  return (VInl v)
denoteCBVal env (Inr e) = do
  v <- thunkCBVal (denoteCBVal env e)
  return (VInr v)
denoteCBVal env (Pm e (Inl (Lambda x body1)) (Inr (Lambda y body2))) = do 
  v <- denoteCBVal env e
  case v of
    VInl val -> denoteCBVal (extendEnv x val env) body1
    VInr val -> denoteCBVal (extendEnv y val env) body2
    _ -> undefined
denoteCBVal env (Print e1 e2) = do
  v1 <- denoteCBVal env e1
  printf v1
  denoteCBVal env e2
denoteCBVal _ _ = undefined

emptyEnvCBVal :: Env (Value (Operation + State [String]))
emptyEnvCBVal = Env []

extendEnv :: String -> CBVal s -> Env s -> Env s
extendEnv x val (Env env) = Env ((x, val) : env)