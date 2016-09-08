module Chap13 where

import Control.Monad
import System.Exit (exitSuccess)
import Data.Char
import Data.Maybe

palindrome :: IO()
palindrome = forever $ do
  line1 <- getLine
  let line = filterSymbols line1 in
    case (line == reverse line) of
      True -> do
        putStrLn "It's a palindrome"
        exitSuccess
      False -> putStrLn "Nope!"
  
filterSymbols :: String -> String
filterSymbols s = catMaybes $ fmap f s
  where f c = case isLower (toLower c) of
          True -> Just (toLower c)
          False -> Nothing

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
    "Name was: " ++ show name ++
    "Agewas:"++ show age

gimmePerson :: IO()
gimmePerson = do
  putStrLn "Enter your name:"
  name <- getLine
  putStrLn "Enter your age:"
  age <- getLine
  putStrLn (reportPersonError name (read age))
  return()
    
  
reportPersonError name age = case (mkPerson name age) of
  (Right p@(Person _ _)) -> "Yay! Successfully got a person:" ++ (show p)
  (Left NameEmpty) -> "Error occured: Name is empty"
  (Left AgeTooLow) -> "Error occured: Age too low"
