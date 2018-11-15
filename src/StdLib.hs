module StdLib where

-- import Data.Map.Strict (fromList)
import Interpreter

withNum:: (Int -> Value) -> Value -> M Value
withNum fn (Num v) = pure . fn $ v
withNum fn _ = wrong ExpectedNumber

neg:: Value
neg = Fun . withNum $ \i -> Num (-i)


mul:: Value
mul = Fun . withNum $ \a -> Fun . withNum $ \b -> Num (a*b)


stdlib:: [(Name, Value)]
stdlib =
    [ ("neg", neg)
    , ("mul", mul)
    ]