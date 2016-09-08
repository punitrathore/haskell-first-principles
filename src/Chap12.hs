module Chap12 where

import Data.List
import Data.Char

type Name = String
type Age = Integer
data Person = Person Name Age deriving (Show, Eq)

data PersonInvalid = NameBlank | AgeTooLow deriving (Show, Eq)

-- mkPerson :: Name -> Age -> Either PersonInvalid Person
-- mkPerson name age
--   | name /= "" && age >=0 = Right $ Person name age
--   | name == "" = Left NameBlank
--   | age < 0 = Left AgeTooLow


type ValidatePerson a = Either [PersonInvalid] a    

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = case age >=0 of
  True -> Right age
  False -> Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = case name /= "" of
  True -> Right name
  False -> Left [NameBlank]

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson' (Right name) (Right age) = Right (Person name age)
mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPerson' (Left badName) _ = Left badName
mkPerson' _ (Left badAge) = Left badAge


notThe :: String -> Maybe String
notThe "the" = Nothing
notThe w = Just w

replaceThe' :: [String] -> [String]
replaceThe' [] = []
replaceThe' (w:ws) = case notThe w of
  Just w' -> [w'] ++ replaceThe' ws
  Nothing -> ["a"] ++ replaceThe' ws

replaceThe:: String -> String
replaceThe s = unwords $ replaceThe' (words s)

vowelStr = "aeiou"

countTheBeforeVowel' :: [String] -> Integer
countTheBeforeVowel' [] = 0
countTheBeforeVowel' (w:ws) = case notThe w of
  Just _ -> countTheBeforeVowel' ws
  Nothing -> case (elem (head $ head ws) vowelStr) of
    True -> 1 + countTheBeforeVowel' ws
    False -> countTheBeforeVowel' ws

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = countTheBeforeVowel' $ words s

countVowels :: String -> Integer
countVowels [] = 0
countVowels (l:ls) = case (elem (toLower l) vowelStr) of
  True -> 1 + countVowels ls
  False -> countVowels ls

countConsonents :: String -> Integer
countConsonents [] = 0
countConsonents (l:ls) = case (elem (toLower l) vowelStr) of
  True -> countConsonents ls
  False -> 1 + countConsonents ls

newtype Word' = Word' String deriving (Eq, Show)

isWord :: String -> Bool
isWord w = case (countVowels w > countConsonents w) of
  True -> False
  False -> True

mkWord :: String -> Maybe Word'
mkWord w = case isWord w of
  True -> Just (Word' w)
  False -> Nothing

data Nat = Zero
         | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + (natToInteger n)


integerToNat' :: Integer -> Nat
integerToNat' 0 = Zero
integerToNat' n = Succ (integerToNat' (n -1))

integerToNat :: Integer -> Maybe Nat
integerToNat n = Just (integerToNat' n)

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee init _ Nothing = init
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just v) = v


listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe [a] = Just a
listToMaybe _ = Nothing

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes ((Just x):xs) = x:(catMaybes xs)

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr f (Just [])
  where f (Just x) (Just xs) = Just(x:xs)
        f _ _ = Nothing

lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where f (Right a) y = y
        f (Left a) y = a:y

rights' :: [Either a b] -> [b]
rights' = foldr f []
  where f (Right a) y = a:y
        f (Left a) y = y

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' e = foldr f ([],[]) e
  where f (Left a) (xs, ys) = (a:xs, ys)
        f (Right a) (xs, ys) = (xs, a:ys)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f1 _ (Left a) = f1 a
either' _ f2 (Right b) = f2 b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f e@(Left a) = either' (\x -> Nothing) (\x -> Just (f x)) e
eitherMaybe'' f e@(Right a) = either' (\x -> Nothing) (\x -> Just (f x)) e

-- take 10 $ iterate (+1) 0
myIterate :: (a -> a) -> a -> [a]
myIterate f init = init:(myIterate f v)
  where v = f init


-- take 10 $ unfoldr (\b -> Just (b, b+1)) 0
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f init = a:(myUnfoldr f b)
  where (Just (a, b)) = f init        

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\a -> Just(a, f a)) x

data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

-- Leaf 0 Leaf
unfold:: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f a = case f a of
  (Just (lt, x, rt)) -> Node (unfold f lt) x (unfold f rt)
  Nothing -> Leaf

treeBuild :: Int -> BinaryTree Int
treeBuild n = unfold (\b -> if b < 2^n - 1
                            then Just (2*b+1, b, 2*b+2)
                            else Nothing) 0


-- treeBuild 0
-- => unfold (\b -> if b < 0
--                              then Just(2*b+1, b, 2*b+2)
--                           else Nothing) 0
-- => Leaf

-- treeBuild 1
-- => unfold (\b -> if b < 1
--                     then Just(2*b+1, b, 2*b+2)
--                     else Nothing) 0
--  => Node( Leaf, 0 , Leaf)

