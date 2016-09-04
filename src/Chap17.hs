module Chap17 where

import Control.Applicative (ZipList, liftA2)
import Data.List (elemIndex)
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y  <*> z

x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y1 :: Maybe Int
y1 = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max
maxed :: Maybe Int
maxed = max' <$> x <*> y1


xs = [1, 2, 3]
ys = [4, 5, 6]
x1 :: Maybe Integer
x1 = lookup 3 $ zip xs ys
y2 :: Maybe Integer
y2 = lookup 2 $ zip xs ys
summed :: Maybe Integer
summed = sum <$> ((,) <$> x1 <*> y2)


newtype Identity a = Identity a deriving (Eq, Ord, Show)
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)


newtype Constant a b = Constant { getConstant :: a }
                     deriving (Eq, Ord, Show)
instance Functor (Constant a) where
  fmap f (Constant x) = Constant x
instance Monoid a => Applicative (Constant a) where
  pure a = (pure a)
  (<*>) (Constant f) (Constant a)  = Constant (f <> a)

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if (length s) > maxLen then Nothing
  else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a       

data Person = Person Name Address deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a = Person <$> (mkName n) <*> (mkAddress a)

data Cow = Cow {
  name :: String,
  age :: Int,
  weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name' age' weight' =
  Cow <$> (noEmpty name')
      <*> (noNegative age')
      <*> (noNegative weight')


instance Monoid a => Monoid (ZipList a) where
  mempty = pure mempty
  mappend = liftA2 mappend

-- instance Applicative ZipList where
--   (<*>) (ZipList f) (ZipList x) = ZipList (f x)

instance Eq a => EqProp (ZipList a) where
  (=-=) = eq


data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons a rem) = Cons a (take' (n -1) rem)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

-- flatMap :: (a -> List b) -> List a -> List b
-- flatMap f (Cons x xs) = concat' (Cons (f x) (flatMap f xs))

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

instance Applicative List where
  pure a = pure a
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) c1@(Cons f fl) c2 = append (fmap f c2) (fl <*> c2)

newtype ZipList' a = ZipList' (List a)
                   deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure = pure
  (<*>) l1@(ZipList' fs) l2@(ZipList' vs) = ZipList' (fs <*> vs)

-- testZipList' = do
--   quickBatch $ applicative[(ZipList' Nil,"2","3")]
