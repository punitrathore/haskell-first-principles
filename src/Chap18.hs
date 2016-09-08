module Chap18 where

import Control.Monad
import Control.Applicative
import Data.Monoid hiding(Sum, First)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

bind :: Monad m => (a -> m b) -> m a -> m b
bind f a = join $ fmap f a

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x] else []


data Cow = Cow {
  name :: String
  , age :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499 then Nothing
     else Just c

mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' n a w = do
  n' <- noEmpty n
  a' <- noNegative a
  w' <- noNegative w
  weightCheck(Cow n' a' w')
  


f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n
g :: Integer -> Maybe Integer
g i =
  if even i
  then Just (i + 1) else Nothing

h :: Integer -> Maybe String
h i = Just ("10191" ++ show i)


doSomething = do
  a <- f 0
  b <- g a
  c <- h b
  pure(a,b,c)

doSomething' n = do
  a <- f n
  b <- g a
  c <- h b
  return (a, b, c)


data Sum a b = First a
             | Second b
             deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First a) = (First a)
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure x = Second x
  (<*>) (First a) _ = First a
  (<*>) _ (First a) = First a
  (<*>) (Second f) (Second v) = Second (f v)

instance Monad (Sum a) where
  return = pure
  (>>=) (First a) f = (First a)
  (>>=) (Second v) f = (f v)

validateAge :: Int -> Sum String Int
validateAge n
  | n > 120 = First "You can't live this long"
  | otherwise = Second n

foo a  = do
  f1 <- validateAge a
  if f1 > 50
     then First "You should be retired"
     else Second "You have many more years to go!"


data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap f NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure x = NopeDotJpg
  (<*>) NopeDotJpg NopeDotJpg = NopeDotJpg

instance Monad Nope where
  return = pure
  NopeDotJpg >>= f = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

testNope = do
  quickBatch $ applicative (undefined :: Nope (Int, Double, Char))
  quickBatch $ monad (undefined :: Nope (Int, Double, Char))


data PhhhbbtttEither b a = Left' a
                         | Right' b
                           deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Left' a) = Left' (f a)
  fmap f (Right' b) = Right' b
                           
instance Applicative (PhhhbbtttEither b) where
  pure a = Left' a
  Left' f <*> Left' v = Left' (f v)
  Right' b <*> _ = Right' b
  _ <*> Right' b = Right' b

instance Monad (PhhhbbtttEither b) where
  return = pure
  (Left' v) >>= f = f v
  (Right' a) >>= _ = Right' a

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    frequency [(1, liftM Left' arbitrary),
               (1, liftM Right' arbitrary)]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

testPhhhbbtttEither = do
  let t = (undefined :: (PhhhbbtttEither Int (Int, Int, Int)))
  quickBatch $ functor t
  quickBatch $ applicative t
  quickBatch $ monad t


newtype Identity a = Identity a 
                   deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity v) = Identity (f v)

instance Monad Identity where
  return = pure
  (Identity v) >>= f = f v

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = liftM Identity arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

testIdentity = do
  let t = (undefined :: (Identity (Int, Int, Int)))
  quickBatch $ functor t
  quickBatch $ applicative t
  quickBatch $ monad t


data List a = Nil
            | Cons a (List a)
              deriving (Eq, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a l) = (Cons (f a) (fmap f l))

instance Monoid (List a) where
  mempty = Nil
  mappend Nil x = x
  mappend x Nil = x
  mappend (Cons x xs) ys = (Cons x (xs `mappend` ys))


instance Applicative List where
  pure x = (Cons x Nil)
  (<*>) (Cons f fl) vs = (fmap f vs) `mappend` (fl <*> vs)
  (<*>) _ Nil = Nil  
  (<*>) Nil _ = Nil

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  (Cons a l) >>= f = (f a) <> (l >>= f)

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency[(1, return Nil),
              (1, return(Cons x y))]

testList = do
  let t = undefined :: List (Int, String, Double)
  quickBatch $ applicative t
  quickBatch $ applicative t
  quickBatch $ monad t
b

j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f a b = f <$> a <*> b

a :: Monad m => m a -> m (a -> b) -> m b
a = flip(<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = pure []
meh (x:xs) f = (:) <$> (f x) <*> (meh xs f)

flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id