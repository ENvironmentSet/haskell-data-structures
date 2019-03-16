module LinkedList ( LinkedList(..), remove, insert ) where

data LinkedList a = Nil | Cons a (LinkedList a) deriving (Show)

instance Monoid (LinkedList a) where
  mempty = Nil
  Nil `mappend` x = x
  x `mappend` Nil = x
  Cons x Nil `mappend` y = Cons x y
  Cons x xs `mappend` y = Cons x (xs `mappend` y)

instance Functor LinkedList where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative LinkedList where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f fs <*> xs = fmap f xs `mappend` (fs <*> xs)

instance Monad LinkedList where
  return = pure
  Nil >>= f = Nil
  Cons x xs >>= f = f x `mappend` (xs >>= f)

remove :: Int -> LinkedList a -> LinkedList a
remove _ Nil = Nil
remove 0 (Cons x xs) = xs
remove p (Cons x xs) = Cons x (remove (p - 1) xs)

insert :: a -> Int -> LinkedList a -> LinkedList a
insert _ _ Nil = Nil
insert v 0 xs = Cons v xs
insert v p (Cons x xs) = Cons x (insert v (p - 1) xs)
