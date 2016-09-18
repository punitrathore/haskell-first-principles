{-# LANGUAGE InstanceSigs #-}
module Chap22 where

import Control.Monad

newtype Reader r a = Reader { runReader :: r -> a }

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
  fmap f (Reader b) = Reader $ (f.b)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ pure a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> (rab r) (ra r)


newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person {
  humanName :: HumanName ,
  dogName :: DogName ,
  address :: Address
  } deriving (Eq, Show)

data Dog = Dog {
  dogsName :: DogName ,
  dogsAddress :: Address
  } deriving (Eq, Show)


pers :: Person
pers = Person
  (HumanName "Big Bird")
  (DogName "Barkley")
  (Address "Sesame Street")

chris :: Person
chris = Person
  (HumanName "Chris Allen")
  (DogName "Papu")
  (Address "Austin")

getDogR :: Reader Person Dog
getDogR = Reader $ Dog <$> dogName <*> address

getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy

instance Monad (Reader r) where
  return :: a -> Reader r a
  return a = Reader $ pure a

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (>>=) (Reader ra) aRb = Reader $ (\r -> runReader (aRb (ra r)) r)


getDogRM' :: Reader Person Dog
getDogRM' = Reader $ liftM2 Dog dogName address