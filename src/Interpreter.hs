module Interpreter where

import qualified Data.Map.Strict as M
import Data.Functor.Identity (Identity)

type Name = String

data Error =
    UnboundVar Name
    | ExpectedNumber
    | ExpectedFunction
    | Custom String
    deriving (Eq, Show)

data Result a = Ok a | Err ([Position], Error) deriving (Eq, Show)

instance Functor Result where
    fmap f (Ok a) = Ok (f a)
    fmap f (Err e) = Err e

instance Applicative Result where
    pure a = Ok a
    (Ok fn) <*> (Ok v) = Ok (fn v)
    Err err <*> _ = Err err
    fn <*> Err err = Err err

instance Monad Result where
    Ok a >>= f = f a
    Err err >>= f = Err err

newtype Position = Label String
    deriving (Eq, Show)

newtype ST a = ST ([Position] -> ([Position], Result a))

runST:: ST a -> [Position] -> ([Position], Result a)
runST (ST a) tb = a tb

instance Functor ST where
    fmap f (ST v) = ST $ go
        where
            go tb = let (ntb, r) = v tb in (ntb, f <$> r)
        -- \tb -> (tb, f <$> v tb)

instance Applicative ST where
    pure r = ST $ \tb -> (tb, pure r)
    (ST fn) <*> (ST arg) = ST $ go
        where
            go tb =
                let 
                    (tb1, fn1) = fn tb
                    (tb2, arg1) = arg tb1
                in
                    (tb2, fn1 <*> arg1)
        -- \tb -> (fn tb) <*> (arg tb)

instance Monad ST where
    (ST a) >>= f = ST $ go
        where
            go tb =
                let
                    (tb1, a1) = a tb
                in
                    case f <$> a1 of
                        Err err -> (tb1, Err err)
                        Ok g -> runST g tb1


enterLabel:: Position -> ST ()
enterLabel pos = ST $ \tb -> (pos:tb, Ok ())

exitLabel:: ST()
exitLabel = ST $ \tb -> (tail tb, Ok ())

type M = ST

wrong:: Error -> M a
wrong err = ST $ \tb -> (tb, Err (tb, err))

data Value = TrueV | FalseV | Num Int | Fun (Value -> M Value)

instance Eq Value where
    -- Wrong == Wrong = True
    Num a == Num b = a==b
    _ == _ = False

data Term =
    Var Name
    | Const Int
    | ConstBool Bool
    | Add Term Term
    | Lam Name Term
    | App Term Term
    | Lbl Position Term
    | FailWith Error
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
interp (ConstBool b) env = pure $ if b then TrueV else FalseV
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
interp (Lbl lbl term) env = do
    enterLabel lbl
    v <- interp term env
    exitLabel
    return v
interp (FailWith err) env = wrong err

