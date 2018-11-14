module Interpreter where

import qualified Data.Map.Strict as M
import Data.Functor.Identity (Identity)

type Name = String

data Error =
    UnboundVar Name
    | ExpectedNumber
    | ExpectedFunction
    deriving (Eq, Show)

type M = Either Error


wrong:: Error -> M a
wrong err = Left err

data Value = Num Int | Fun (Value -> M Value)

instance Eq Value where
    -- Wrong == Wrong = True
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
    -- show Wrong =  "<wrong>"
    show (Num v) = show v
    show (Fun _) = "<function>"


fromMaybe:: Error -> Maybe a -> M a
fromMaybe msg v = case v of
    Just a -> pure a
    Nothing -> wrong msg


interp:: Term -> Env -> M Value
interp (Var name) env = fromMaybe (UnboundVar name) $ M.lookup name env
interp (Const val) env = pure $ Num val
interp (Add t1 t2) env = do
        a <- interp t1 env
        b <- interp t2 env
        add a b
    where
        add (Num v1) (Num v2) = pure $ Num (v1+v2)
        add _ _ = wrong ExpectedNumber
interp (Lam name body) env = pure (Fun f)
    where
        f v = interp body $ M.insert name v env
interp (App fn arg) env = do
    fn <- interp fn env
    v <- interp arg env
    case fn of
        Fun ff -> ff v
        _ -> wrong ExpectedFunction
