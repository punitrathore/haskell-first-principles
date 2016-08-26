module Arith where

import Data.Char

add::Int->Int->Int
add x y=x+y
addPF :: Int -> Int -> Int
addPF = (+)
addOne :: Int -> Int
addOne = \x->x+1
addOnePF :: Int -> Int
addOnePF = (+1)

main :: IO ()
main = do
 print (0 :: Int)
 print (add 1 0)
 print (addOne 0)
 print (addOnePF 0)
 print((addOne .addOne)0)
 print ((addOnePF . addOne) 0)
 print((addOne .addOnePF) 0)
 print ((addOnePF . addOnePF) 0)
 print (negate (addOne 0))
 print ((negate . addOne) 0)
 print ((addOne . addOne . addOne . negate . addOne) 0)

g::(a->b)->(a,c)->(b,c)
g f (a,c) = ((f a),c)


myTail:: [t] -> Maybe [t]
myTail [] = Nothing
myTail (_:[]) = Nothing
myTail (_:xs) = Just xs

myHead:: [t] -> Maybe t
myHead [] = Nothing
myHead (x:[]) = Just x
myHead (x:_) = Just x


eftBool :: Bool -> Bool -> [Bool]
eftBool False True = [False, True]
eftBool False False = [False]
eftBool True True = [True]
eftBool True False = []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y = case (x > y) of
  True -> []
  False -> x:(eftOrd (succ x) y)

eftInt :: Int -> Int -> [Int]
eftInt x y = case (x > y) of
  True -> []
  False -> x:(eftInt (succ x) y)

eftChar :: Char -> Char -> [Char]
eftChar x y = case (x > y) of
  True -> []
  False -> x:(eftChar (succ x) y)

-- ["p r"] -> ["p","r"]
-- ["pun r"] -> ["pun" "r"]

myWords :: [Char] -> [[Char]]
myWords [] = []
myWords (' ':xs) = myWords xs
myWords xs = f:myWords(r)
  where f = takeWhile (/=' ') xs
        r = dropWhile (/=' ') xs


mySplit :: Char -> [Char] -> [[Char]]
mySplit _ [] = []
mySplit c xa@(x:xs) = case (x == c) of
  True -> mySplit c xs
  False -> f:(mySplit c r)
    where f = takeWhile (/= c) xa
          r = dropWhile (/= c) xa

myWords2 = mySplit ' '

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

mySentences = mySplit '\n'

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

myLength :: [a] -> Integer
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myZip :: [a] -> [b] -> [(a,b)]
myZip [] [] = []
myZip [] ys = []
myZip xs [] = []
myZip (x:xs) (y:ys) = (x,y):(myZip xs ys)

capStr :: [Char] -> [Char]
capStr "woot" = "WOOT"
capStr [] = []
capStr (x:xs) = (toUpper x):xs


capStr2 :: [Char] -> Char
capStr2 = toUpper . head


