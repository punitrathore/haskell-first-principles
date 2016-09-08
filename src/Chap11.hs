module Chap11 where

import Data.Char
import Data.List

computeShift :: Char -> Int
computeShift c = ord c - ord 'A' 

encodeChar :: Char -> Char -> Char
encodeChar ' ' _  = ' '
encodeChar c e = chr (ord(c) + shift)
  where shift = computeShift e

encode :: String -> String
encode = map (\x -> x)

-- foo :: String -> String -> String
-- foo s cipher = foldr () "" s

-- cipher = "ALLY"

-- isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
-- isSubsequenceOf [] _ = True
-- isSubsequenceOf _ [] = False
-- isSubsequenceOf x'@(x:xs) (y:ys) = case x == y of
--   True -> isSubsequenceOf xs ys
--   False -> isSubsequenceOf x' ys


capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper(x):xs

capitalizeWords :: String -> [(String, String)]
capitalizeWords s = map (\x -> (x,capitalizeWord x)) $ words s

capitalizeParagraph :: String -> String
capitalizeParagraph p = unwords $ map capitalizeWord $ words p

type PhoneDigit = Char
type PhoneChar = Char

data Key = Key PhoneDigit [PhoneChar] deriving(Eq, Show)

data DaPhone = DaPhone [Key] deriving (Eq, Show)

phone = DaPhone [(Key '1' ""),
                 (Key '2' "abc"),
                 (Key '3' "def"),
                 (Key '4' "ghi"),
                 (Key '5' "jkl"),
                 (Key '6' "mno"),
                 (Key '7' "pqrs"),
                 (Key '8' "tuv"),
                 (Key '9' "wxyz"),
                 (Key '*' "^"),
                 (Key '0' " +"),
                 (Key '#' ".,")]

type Digit = Char
-- Valid presses: 1 and up
type Presses = Int

-- findKey phone 'A' -> [('2', 1)]

findKey :: DaPhone -> Char -> Key
findKey (DaPhone (theKey@(Key digit vals):ks)) k = case digit == k of
  True -> theKey
  False -> findKey (DaPhone ks) k  

findDigitPresses :: DaPhone -> Char -> [(Digit, Presses)]
findDigitPresses phone@(DaPhone (theKey@(Key digit vals):ks)) k = case isUpper k of
  True -> [('*', 1)] ++ findDigitPresses phone (toLower k)
  False -> case (elemIndex k vals) of
    Just(idx) -> [(digit, idx + 1)]
    Nothing -> findDigitPresses (DaPhone ks) k

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone c = findDigitPresses phone c

convo :: [String]
convo = ["Wanna play 20 questions",
         "Ya",
         "U 1st haha",
         "Lol ok. Have u ever tasted alcohol lol",
         "Lol ya",
         "Wow ur cool haha. Ur turn",
         "Ok. Do u think I am pretty Lol",
         "Lol ya",
         "Haha thanks just making sure rofl ur turn"]

flatten = foldr (\x y -> x ++ y) []

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone w = flatten $ map (reverseTaps phone) w


fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps [] = 0
fingerTaps ((digit, presses):r) = presses + fingerTaps r

-- mostPopularLetters :: String -> Char



data Expr = Lit Integer
          | Add Expr Expr

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add e1 e2) = (eval e1) + (eval e2)

printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add e1 e2) = (printExpr e1) ++ " + " ++ (printExpr e2)
