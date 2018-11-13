module Interpreter where

import qualified Data.Map.Strict as M

type Name = String

data Value = Wrong | Num Int | Fun (Value -> Value)

instance Eq Value where
    Wrong == Wrong = True
    Num a == Num b = a==b
    _ == _ = False

data Term =
    Var Name
    | Const Int
    | Add Term Term
    | Lam Name Term
    | App Term Term

type Env = M.Map Name Value

instance Show Value where
    show Wrong =  "<wrong>"
    show (Num v) = show v
    show (Fun _) = "<function>"


interp:: Term -> Env -> Value
interp (Var name) env = M.findWithDefault Wrong name env
interp (Const val) env = Num val
interp (Add t1 t2) env =
    let
        v1 = interp t1 env
        v2 = interp t2 env
    in
        case (v1, v2) of
            (Num n1, Num n2) -> Num (n1+n2)
            _ -> Wrong
interp (Lam name body) env = Fun $ \v -> interp body $ M.insert name v env
interp (App fn arg) env = case interp fn env of
    Fun ff -> ff (interp arg env)
    _ -> Wrong
