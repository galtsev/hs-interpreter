module Interpreter where

import qualified Data.Map.Strict as M
import Interp.Result
import Interp.State

type Name = String

data Error =
    UnboundVar Name
    | ExpectedNumber
    | ExpectedFunction
    | Custom String
    deriving (Eq, Show)

newtype Position = Label String
    deriving (Eq, Show)


-- enterLabel:: Position -> ST ()
-- enterLabel pos = ST $ \tb -> (pos:tb, Ok ())

-- exitLabel:: ST()
-- exitLabel = ST $ \tb -> (tail tb, Ok ())

type M = ResultT ([Position], Error) (State [Position])

wrong:: Error -> M a
-- wrong err = ST $ \tb -> (tb, Err (tb, err))
wrong err = ResultT . State $ \tb -> (tb, Err (tb, err))

enterLabel:: Position -> M ()
enterLabel pos = ResultT . State $ \tb -> (pos:tb, Ok ())

exitLabel:: M ()
exitLabel = ResultT . State $ \tb -> (tail tb, Ok ())

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

