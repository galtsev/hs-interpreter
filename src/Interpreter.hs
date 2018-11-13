module Interpreter where

import qualified Data.Map.Strict as M
import Data.Functor.Identity (Identity)

type Name = String

type M = Identity

data Value = Wrong | Num Int | Fun (Value -> M Value)

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
    deriving (Eq, Show)

type Env = M.Map Name Value

instance Show Value where
    show Wrong =  "<wrong>"
    show (Num v) = show v
    show (Fun _) = "<function>"


interp:: Term -> Env -> M Value
interp (Var name) env = pure $ M.findWithDefault Wrong name env
interp (Const val) env = pure $ Num val
interp (Add t1 t2) env = add <$> interp t1 env <*> interp t2 env
    where
        add (Num v1) (Num v2) = Num (v1+v2)
        add _ _ = Wrong
interp (Lam name body) env = pure (Fun f)
    where
        f v = interp body $ M.insert name v env
interp (App fn arg) env = do
    fn <- interp fn env
    v <- interp arg env
    case fn of
        Fun ff -> ff v
        _ -> pure Wrong
