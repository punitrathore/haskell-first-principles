module Chap17 where

import Control.Applicative (ZipList, liftA2, liftA3)
import Data.List (elemIndex)
import Data.Monoid
import Control.Monad
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


-- instance Monoid a => Monoid (ZipList a) where
--   mempty = pure mempty
--   mappend = liftA2 mappend

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
  pure a = Cons a Nil
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) c1@(Cons f fl) c2 = append (fmap f c2) (fl <*> c2)


instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = Cons <$> arbitrary <*> arbitrary

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

instance (Arbitrary a) => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

repeatList :: a -> (List a)
repeatList x = xs
  where xs = Cons x xs

zipListWith :: (a -> b -> c) -> (List a) -> (List b) -> (List c)
zipListWith _ Nil _ = Nil
zipListWith _ _ Nil = Nil
zipListWith f (Cons a as) (Cons b bs) = Cons (f a b) (zipListWith f as bs)

instance Applicative ZipList' where
  pure x = ZipList' (repeatList x)
  ZipList' fs <*> ZipList' xs = ZipList' (zipListWith id fs xs)


-- testZipList' = do
--   quickBatch $ applicative[(ZipList' Nil,"2","3")]

data Sum' a b = First' a
             | Second' b
             deriving (Eq, Show)

instance Functor (Sum' a) where
  fmap f (First' a) = First' a
  fmap f (Second' a) = Second' (f a)

instance Applicative (Sum' a) where
  pure = Second'
  (<*>) (First' a) _ = First' a
  (<*>) _ (First' a) = First' a
  (<*>) (Second' a) (Second' b) = Second' (a b)

data Validation e a = Error e
                    | Success' a deriving (Eq, Show)

instance (Monoid e) => Monoid (Validation e a) where
  mempty = mempty
  mappend (Error e1) (Error e2) = Error(e1 <> e2)
  mappend (Error e1) _ = Error e1
  mappend _ (Error e1) = Error e1
                      
instance Functor (Validation e) where
  fmap f (Error e) = Error e
  fmap f (Success' a) = Success' (f a)

instance (Monoid e) => Applicative (Validation e) where
  pure = Success'
  (<*>) (Error e) (Error e') = Error (e <> e')
  (<*>) (Error e) _ = Error e
  (<*>) _ (Error e) = Error e
  (<*>) (Success' f) (Success' v) = Success' (f v)



------
-- newtype Identity a = Identity a deriving Show

-- instance Applicative Identity where
--   pure = Identity
--   (<*>) (Identity f) (Identity v) = Identity (f v)

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)
    
testIdentity =  quickBatch $ applicative (undefined :: Identity (Int, Double, Char))


data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure x = Pair x x
  (<*>) (Pair x1 y1) (Pair x2 y2) = (Pair (x1 x2) (y1 y2))

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = liftM2 Pair arbitrary arbitrary

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

testPair = quickBatch $ applicative (undefined:: Pair (Int, Double, Char))

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Monoid a) => Applicative (Two a) where
  pure x = Two mempty x
  (<*>) (Two a1 f1) (Two a2 b2) = Two (a1 <> a2) (f1 b2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

testTwo = quickBatch $ applicative (undefined :: Two String (Int, Double, Char))

----------------

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four w x y z) = Four w x y (f z)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x
  (<*>) (Four w1 x1 y1 f) (Four w2 x2 y2 z) = Four (w1 <> w2) (x1 <> x2) (y1 <> y2) (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    x1 <- arbitrary
    x2 <- arbitrary
    x3 <- arbitrary
    x4 <- arbitrary
    return (Four x1 x2 x3 x4)

    
instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

testFour = quickBatch $ applicative (undefined :: (Four String String String (Int, Char, Double)))


----------------

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance (Monoid a) => Applicative (Four' a) where
  pure x = (Four' mempty mempty mempty x)
  (<*>) (Four' x1 y1 z1 f) (Four' x2 y2 z2 v) = Four' (x1 <> x2) (y1 <> y2) (z1 <> z2) (f v)

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a1 <- arbitrary
    b1 <- arbitrary
    return (Four' a1 a1 a1 b1)

testFour' = quickBatch $ applicative (undefined :: Four' String (Int, Double, Char))


stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos a b c = liftA3 (,,) a b c
