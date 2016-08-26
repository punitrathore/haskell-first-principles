module Sing where
import Data.List

-- fstString :: [Char] -> [Char]
-- fstString x = x ++ " in the rain"

-- sndString :: [Char] -> [Char]
-- sndString x = x ++ " over the rainbow"

-- sing = if (x < y) then fstString x else sndString y
--   where x = "Singin"
--         y = "Somewhere"


-- main :: IO ()
-- main =
--   do
--     print (1 + 2)
--     putStrLn "10"
--     print (negate 1)
--     print ((+) 0 blah)
--       where blah = negate 1

-- divideThenAdd :: (Fractional a, Num a) => a -> a -> a
-- divideThenAdd x y = (x / y ) + 1

-- data Mood = Blah
-- instance Show Mood where
--   show _ = "Blah"

-- data Trivial = Trivial' deriving Show

-- instance Eq Trivial where
--   (==) Trivial' Trivial' = True

data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun
               deriving (Show, Ord)
data Date = Date DayOfWeek Int

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

instance Eq Date where
  (==) (Date dayOfWeek day)
       (Date dayOfWeek' day') = (dayOfWeek' == dayOfWeek) && (day == day')
  

data TisAnInteger = TisAn Integer
instance Eq TisAnInteger where
  (==) (TisAn i) (TisAn i') = (i == i')

data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
  (==) (Two i1 i2) (Two i1' i2') = (i1 == i1') && (i2 == i2')

data StringOrInt = TisAnInt Int
                 | TisAString String
instance Eq StringOrInt where
  (==) (TisAnInt i) (TisAnInt i') = (i == i')
  (==) (TisAString s) (TisAString s') = (s == s')
  (==) _ _ = False
  

data Pair a = Pair a a
instance Eq a => Eq (Pair a) where
  (==) (Pair a b) (Pair a' b') = (a == a') && (b == b')

data Tuple a b = Tuple a b deriving Show
instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') = (a == a') && (b == b')

data EitherOr a b = Hello a
                  | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a') = (a == a')
  (==) (Goodbye b) (Goodbye b') = (b == b')
  (==) _ _ = False


--- tychecking exercises
data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood = Blah
          | Woot deriving (Show, Ord, Eq)

settleDown x = if x == Woot then Blah
               else x

type Subject = String
type Verb = String
type Object = String
data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "punit" "rathore" "reads"

data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

s4 = Papu (Rocks "foo") (Yeah True)


f:: Int -> Int
f x = x

mySort :: [Char] -> [Char]
mySort = sort

-- signifier :: Ord a => [a] -> a
-- signifier xs = head (mySort xs)


chk::Eq b => (a -> b) -> a -> b -> Bool
chk f a b = (f a) == b

-- foo:: (a -> a) -> a -> a
-- foo f x = f(x)

-- arith :: Num b => ( a -> b) -> Integer -> a -> b
-- arith :: Num a => ( a -> b) -> b -> a -> b
arith f i a = (f a) * i

-- mTh x y = \z -> x * y * z
mTh x =  (\y -> \z -> x * y * z)


addOne = (\x -> x+1)::Integer -> Integer

addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1

addFive::Integer->Integer -> Integer
addFive = \x -> \y -> (if x > y then y else x)+5

data WherePenguinsLive = India | SouthAfrica | Zimbabwe

data Penguin = Peng WherePenguinsLive

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

f2::(a,b,c)->(d,e,f)->((a,d),(c,f))
f2 (a,b,c) (d,e,f) = ((a,d),(c,f))

myAbs::Integer -> Integer
myAbs x = case (x > 0) of
  True -> x
  _ -> -x

myAbs2::Integer -> Integer
myAbs2 x 
  | x > 0 = x
  | x < 0 = -x