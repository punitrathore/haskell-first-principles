module Chap20 where

import Prelude hiding(Right, Left, Either)

sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0

product :: (Foldable t, Num a) => t a -> a
product = foldr (*) 1

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem  = any . (==)


data Constant a b = Constant a deriving(Eq, Show)

instance Monoid a => Monoid (Constant a b) where
  mempty = Constant mempty
  mappend (Constant a) (Constant b) = Constant (a `mappend` b)

instance Foldable (Constant a) where
  foldMap _ _ = mempty

data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two a) where
  foldMap f (Two a b) = f b

data Three' a b = Three' a b b deriving(Eq, Show)

instance Foldable (Three' a) where
  foldMap f (Three' a b c) = f c

data Four a b = Four a b b b 

instance Foldable (Four a) where
  foldMap f (Four a b c d) = f d


filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f m = foldMap f' m
  where f' x = if f x
               then mempty
               else pure x


data Expr a = Num Int             -- atom
          | Str String          -- atom
          | Op BinOp (Expr a) (Expr a)  -- compound
            deriving (Show)

data BinOp = Add | Concat
             deriving (Show)

interp x@(Num _)                     = x
interp x@(Str _)                     = x
interp (Op Add a b)                  = Num (i a + i b)
  where i x = case interp x of Num a -> a
interp (Op Concat a b)   = Str (i a ++ i b)
  where i x = case interp x of Str a -> a



data Either a b = Left a
                | Right b
                deriving (Eq, Ord, Show)
instance Functor (Either a) where
  fmap _ (Left x) = Left x
  fmap f (Right y) = Right (f y)

instance Applicative (Either e) where
  pure = Right
  Left e <*> _ = Left e
  Right f <*> r = fmap f r
  
instance Foldable (Either a) where
  foldMap _ (Left _) = mempty
  foldMap f (Right y) = f y

  foldr _ z (Left _) = z
  foldr f z (Right y) = f y z

instance Traversable (Either a) where
  traverse _ (Left x) = pure (Left x)
  traverse f (Right y) = Right <$> f y


  