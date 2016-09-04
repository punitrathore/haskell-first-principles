module Main where

import Data.List (sort)
import Test.QuickCheck
import Data.Char

-- for any list you apply sort to
-- this property should hold

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x,t) = (Just y, x >= y)

prop_listOrdered :: (Ord a) => [a] -> Bool
prop_listOrdered lst = listOrdered lst

-- checkListOrdered = quickCheck prop_listOrdered

twice f = f.f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper(x):xs

f :: String -> Bool
f x =
  capitalizeWord x
  == twice capitalizeWord x

prop_checkCapitalizedWord = quickCheck f

g :: (Ord a) => [a] -> Bool
g x =
  sort x
  == twice sort x

prop_checkSort = quickCheck (g::[String] -> Bool)

main :: IO()
main = undefined

