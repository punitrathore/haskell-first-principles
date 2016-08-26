{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Adt where

import Data.Int

data Doggies a = Husky a
               | Mastiff a deriving (Eq, Show)

data Price = Price Integer deriving (Eq, Show)

data Size = Size String deriving (Eq, Show)

data Manufacturer = Mini
                  | Mazda
                  | Tata
                  deriving (Eq, Show)

data Airline = PapuAir 
             | CatapultsR'Us
             | TakeYourChancesUnited
             deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m

tooManyGoats:: Int -> Bool
tooManyGoats n = n > 42


class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Show, Eq, TooMany)
newtype Cows = Cows Int deriving (Show, Eq, TooMany)

instance TooMany String where
  tooMany s = length s > 42

instance TooMany (Int,String) where
  tooMany (i,s) = tooMany(i + length s)

instance TooMany (Int,Int) where
  tooMany (i1, i2) = tooMany (i1 + i2)

instance TooMany Double where
  tooMany d = d > 42

instance (Num a, TooMany a) => TooMany(a, a) where
  tooMany(a,a') = tooMany(a + a')

  
data NumberOrBool = Numba Int8
                   | BoolyBool Bool deriving (Eq, Show)