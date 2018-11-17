module Interp.Result where

data Result e a = Ok a | Err e deriving (Eq, Show)

instance Functor (Result e) where
    fmap f (Ok a) = Ok (f a)
    fmap f (Err e) = Err e

instance Applicative (Result e) where
    pure a = Ok a
    (Ok fn) <*> (Ok v) = Ok (fn v)
    Err err <*> _ = Err err
    fn <*> Err err = Err err

instance Monad (Result e) where
    Ok a >>= f = f a
    Err err >>= f = Err err

-- -----------
newtype ResultT e m a = ResultT (m (Result e a))

runResultT:: ResultT e m a -> m (Result e a)
runResultT (ResultT ma) = ma

instance Monad m => Functor (ResultT e m) where
    fmap f (ResultT ma) = ResultT (fmap (fmap f) ma)

instance Monad m => Applicative (ResultT e m) where
    pure v = ResultT . pure $ Ok  v
    (ResultT mf) <*> (ResultT ma) = ResultT $ do
        f <- mf
        a <- ma
        pure $ f <*> a

instance Monad m => Monad (ResultT e m) where
    (ResultT ma) >>= f = ResultT $ do
            a <- ma
            case a of
                Err err -> pure (Err err)
                Ok v -> runResultT (f v)

failWith:: Monad m => e -> ResultT e m a
failWith err = ResultT $ pure $ Err err

lift:: Monad m => m a -> ResultT e m a
lift ma = ResultT $ Ok <$> ma
