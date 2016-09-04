module Chap15 where

import Data.Monoid
import Test.QuickCheck
import Control.Monad
import qualified Data.Semigroup as S

data Optional a = Nada | Only a deriving (Eq, Show)

instance (Monoid a) => Monoid (Optional a) where
  mempty = Nada
  mappend Nada x = x
  mappend y Nada = y
  mappend (Only x)  (Only y) = Only(mappend x y)



-- Mad Libbin

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclaimation = String

madlibbin' :: Exclaimation
              -> Adverb
              -> Noun
              -> Adjective
              -> String
madlibbin' e adv n adj =
  mconcat [e, "! he said ",
  adv, " as he jumped into his car ",
  n, " and drove off with this ",
  adj, " wife."]


asc (<>) a b c = a <> (b <> c) == (a <> b) <> c

monoidAssoc a b c = a <> (b <> c) == (a <> b) <> c

monoidLeftIdentity :: (Eq a, Monoid a) => a -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq a, Monoid a) => a -> Bool
monoidRightIdentity a = (a <> mempty) == a


newtype First' a = First' {getFirst:: Optional a}
                 deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' Nada) (First' Nada) = First' Nada
  mappend x'@(First' (Only x)) (First' y) = x'
  mappend (First' Nada) x = x

instance Arbitrary a => Arbitrary (Optional a) where  
  arbitrary  = do
    frequency [ (1, return Nada),
                (2, liftM Only arbitrary)]
    

instance (Arbitrary a) => Arbitrary (First' a) where
  arbitrary = liftM First' arbitrary

    
firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String  -> First' String -> Bool

testFirst' :: IO ()
testFirst' = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: First' String -> Bool)
  quickCheck (monoidRightIdentity :: First' String -> Bool)
  
-- Exercise 2
newtype Identity a = Identity a deriving (Eq, Show)

instance (S.Semigroup a, Monoid a) => S.Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity(x <> y)

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend (Identity x) (Identity y) = Identity(x <> y)

instance (Monoid a, Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = liftM Identity arbitrary

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

testIdentity = do
  quickCheck (monoidAssoc::IdentityAssoc)
  quickCheck (monoidLeftIdentity::Identity String -> Bool)
  quickCheck (monoidRightIdentity::Identity String -> Bool)


--- Exercise 3
data Two a b = Two a b deriving (Eq, Show)

instance (S.Semigroup a, Monoid a, S.Semigroup b, Monoid b) => S.Semigroup (Two a b) where
  (Two x1 y1) <> (Two x2 y2)  = Two (x1 <> x2) (y1 <> y2)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend (Two x1 y1) (Two x2 y2)  = Two (x1 <> x2) (y1 <> y2)

instance (Monoid a, Arbitrary a, Monoid b, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = liftM2 Two arbitrary arbitrary

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

testTwo = do
  quickCheck (monoidAssoc::TwoAssoc)
  quickCheck (monoidLeftIdentity::Two String String -> Bool)
  quickCheck (monoidRightIdentity::Two String String -> Bool)

-- Exercise 5
newtype BoolDisj = BoolDisj Bool deriving (Show, Eq)

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend (BoolDisj False) (BoolDisj False) = BoolDisj False
  mappend (BoolDisj True) _ = BoolDisj True
  mappend _ (BoolDisj True) = BoolDisj True

instance Arbitrary BoolDisj where
  arbitrary = do
    frequency[(1, return (BoolDisj True)),
              (1, return( BoolDisj False))]

testBoolDisj = quickCheck (monoidAssoc::BoolDisj -> BoolDisj -> BoolDisj -> Bool)

--- Exercise 6
newtype Combine a b = Combine { unCombine :: (a -> b) }

-- What it should do:
--      Prelude> let f = Combine $ \n -> Sum (n + 1)
--      Prelude> unCombine (mappend f mempty) $ 1
--      Sum {getSum = 2}

-- instance (Monoid a, Monoid b) => Monoid (Combine a b) where
--   mempty = Combine mempty mempty
--   mappend (Combine a) (Combine b) = Combine (mappend a b)

                                  
-- newtype Mem s a = Mem {
--   runMem :: s -> (a,s)
--   }

-- instance (Monoid s, Monoid a) => Monoid (Mem s a) where
--   mempty = Mem mempty
--   mappend (Mem m1) (Mem m2) = Mem (m1 <> m2)

-- f' = Mem $ \s -> ("hi", s + 1)

-- main = do
--   print $ runMem (f' <> mempty) 0
--  print $ runMem (mempty <> f') 0
--  print $ (runMem mempty 0 :: (String, Int))
--  print $ runMem (f' <> mempty) 0 == runMem f' 0
--  print $ runMem (mempty <> f') 0 == runMem f' 0  
  

