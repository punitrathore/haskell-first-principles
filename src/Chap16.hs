module Chap16 where

import Test.QuickCheck
import Control.Monad
import Data.Char

replaceWithP = const 'p'

lms = [Just "abc", Nothing, Just "def"]

foo = fmap replaceWithP lms
bar = (fmap.fmap) replaceWithP lms
baz = (fmap.fmap.fmap) replaceWithP lms




a = fmap (+1) $ read "[1]" :: [Int]
b = (fmap.fmap) (++ "lol") (Just ["Hi,", "Hello"])
c = fmap (*2) (\x -> x + 2)
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])
-- e :: IO Integer
-- e = let ioi = readIO "1" :: IO Integer
--         changed = fmap ((read . ("123"++)) . show) ioi
--     in (*3) changed


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b)
                  -> (b -> c) -> f a
                  -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)


newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = liftM Identity arbitrary

testIdentity = do
  quickCheck $ \x -> functorIdentity(x::(Identity Int))
  quickCheck $ \x -> functorCompose (*1) (+2) (x::(Identity Int))

data Pair a = Pair a a deriving(Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = liftM2 Pair arbitrary arbitrary

testPair = do
  quickCheck $ \x -> functorIdentity(x::(Pair Int))
  quickCheck $ \x -> functorCompose (*1) (+2) (x::(Pair Int))
  
---------------------------------------------------

data Three a b c = Three a b c deriving(Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftM3 Three arbitrary arbitrary arbitrary

testThree = do
  quickCheck $ \x -> functorIdentity(x::(Three Int Int String))
  quickCheck $ \x -> functorCompose (*1) (+2) (x::(Three Int String Int))
  

data Three' a b = Three' a b b deriving(Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = liftM3 Three' arbitrary arbitrary arbitrary

testThree' = do
  quickCheck $ \x -> functorIdentity(x::(Three' Int Int))
  quickCheck $ \x -> functorCompose (*1) (+2) (x::(Three' Int Int))


data Four' a b = Four' a a a b deriving(Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = (Four' a b c (f d))

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = liftM4 Four' arbitrary arbitrary arbitrary arbitrary

testFour' = do
  quickCheck $ \x -> functorIdentity(x::(Four' Int Int))
  quickCheck $ \x -> functorCompose (*1) (+2) (x::(Four' Int Int))


data Possibly a = LolNope
                | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap f LolNope = LolNope
  fmap f (Yeppers a) = Yeppers (f a)


data Sum a b = First a
             | Second b
             deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First a) = First a
  fmap f (Second a) = Second (f a)


newtype Constant a b = Constant {constant :: a} deriving(Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant b) = Constant b

getInt :: IO Int
getInt = fmap read getLine

data Company a b c = DeepBlue a c
                   | Something b
instance Functor (Company e e') where
  fmap _ (Something b) = Something b
  fmap f (DeepBlue a c) = DeepBlue a (f c)


data More b a = L a b a
              | R b a b
              deriving (Eq, Show)
instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'


data List2 a b = List2 a b deriving(Eq, Show)
instance Functor (List2 a) where
  fmap f (List2 a b) = List2 a (f b)

data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)
instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)


data LiftItOut f a = LiftItOut (f a) deriving(Eq, Show)
instance (Functor t) => Functor (LiftItOut t) where
  fmap f (LiftItOut b) = LiftItOut (fmap f b)


data Parappa f g a = DaWrappa (f a) (g a)  deriving(Eq, Show)
instance (Functor t, Functor t') => Functor (Parappa t t') where
  fmap f' (DaWrappa a b) = DaWrappa (fmap f' a) (fmap f' b)


data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)
instance (Functor t, Functor t') => Functor (IgnoreOne t t' a) where
  fmap f (IgnoringSomething a' b) = IgnoringSomething a' (fmap f b)


data Notorious g o a t = Notorious (g o) (g a) (g t) deriving(Eq, Show)
instance (Functor t1) => Functor (Notorious t1 t2 t3) where
  fmap f (Notorious o a t) = Notorious o a (fmap f t)

data List a = Nil
            | Cons a (List a) deriving(Eq, Show)
instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a' Nil) = Cons (f a') Nil
  fmap f (Cons a' l) = Cons (f a') (fmap f l)
              
data GoatLord a = NoGoat
                | OneGoat a
                | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Functor GoatLord where
  fmap f NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats g1 g2 g3) = MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)


data TalkToMe a = Halt
                | Print String a
                | Read (String -> a)
                  deriving (Eq, Show)

instance Functor TalkToMe where
  fmap f Halt = Halt
--   fmap f (Print String a) = Print String (f a)
--   fmap f (Read g) = Read f.g
         

