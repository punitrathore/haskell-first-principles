module Folds where

import Data.Time

data DatabaseItem = DbString String |
                    DbNumber Integer |
                    DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =[ DbDate (UTCTime
                       (fromGregorian 1911 5 1)
                       (secondsToDiffTime 34123)),
               DbNumber 9001,
               DbNumber 9003,
               DbString "Hello, world!",
               DbDate (UTCTime
                       (fromGregorian 1921 5 1)
                       (secondsToDiffTime 34123))]


filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr f []
  where f dbItem acc = case dbItem of
          (DbDate time) -> time:acc
          _ -> acc

filterDbDate2 :: [DatabaseItem] -> [UTCTime]
filterDbDate2 dbItems = foldl f [] dbItems
  where f acc dbItem = case dbItem of
          (DbDate time) -> time:acc
          _ -> acc



filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber dbItems = foldr f [] dbItems
  where f dbItem acc = case dbItem of
          (DbNumber num) -> num:acc
          _ -> acc

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent dbItems= maximum $ filterDbDate dbItems

sumDb :: [DatabaseItem] -> Integer
sumDb dbItems = sum $ filterDbNumber dbItems

avgDb :: [DatabaseItem] -> Double
avgDb dbItems = let sums = fromIntegral $ sumDb dbItems
                    l = fromIntegral $ length $ filterDbNumber dbItems
                in sums / l

fibs = filter (\x -> x < 100) $ 1 : scanl (+) 1 fibs


stops = "pbtdkg"
vowels = "aeiou"

foo = [(x1,y,x2) | x1 <- stops, y <- vowels, x2 <- stops, x1 == 'p']

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||). f) False

-- myElem::Eq a => a -> [a] -> Bool


myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:).f) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++).f) []

squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = undefined
