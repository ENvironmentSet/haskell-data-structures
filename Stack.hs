module Stack ( Stack(..), push, pop, peek ) where

data Stack a = Top a (Stack a) | Nil deriving(Show)

instance Monoid (Stack a) where
  mempty = Nil
  Nil `mappend` x = x
  x `mappend` Nil = x
  Top x Nil `mappend` y = Top x y
  Top x xs `mappend` y = Top x (xs `mappend` y)

instance Functor Stack where
  fmap _ Nil = Nil
  fmap f (Top x xs) = Top (f x) (fmap f xs)

instance Applicative Stack where
  pure x = Top x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Top f fs <*> xs = fmap f xs `mappend` (fs <*> xs)

instance Monad Stack where
  return = pure
  Nil >>= f = Nil
  Top x xs >>= f = f x `mappend` (xs >>= f)

push :: a -> Stack a -> Stack a
push = Top

pop :: Stack a -> Stack a
pop Nil = Nil
pop (Top x xs) = xs

peek :: Stack a -> Maybe a
peek Nil = Nothing
peek (Top x _) = x
