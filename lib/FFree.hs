module FFree where

data FFree f a where
  Pure :: a -> FFree f a
  Impure :: f x -> (x -> FFree f a) -> FFree f a

instance Functor (FFree f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Impure x k) = Impure x (fmap f . k)

instance Applicative (FFree f) where
  pure = Pure
  Pure f <*> fa = fmap f fa
  Impure x k <*> fa = Impure x (\x' -> k x' <*> fa)

instance Monad (FFree f) where
  Pure a >>= f = f a
  Impure x k >>= k' = Impure x (\x' -> k x' >>= \x'' -> k' x'')
