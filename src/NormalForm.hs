module NormalForm where

-- data Fiction = Fiction deriving Show
-- data Nonfiction = Nonfiction deriving Show
-- data BookType = BookType Fiction
--               | NonfictionBook Nonfiction
--               deriving Show

type AuthorName = String

data Author = Fiction AuthorName
            | Nonfiction AuthorName
              deriving (Eq, Show)

data Expr = Number Int
          | Add Expr Expr
          | Minus Expr
          | Mult Expr Expr
          | Divide Expr Expr

            


-- data FlowerType = Gardenia
--                 | Daisy
--                 | Rose
--                 | Lilac
--                 deriving Show

type Gardener = String

-- data Garden =
--   Garden Gardener FlowerType
--   deriving Show

data Garden = Gardenia Gardener
            | Daisy Gardener
            | Rose Gardener
            | Lilac Gardener
            deriving Show


data GuessWhat = Chickenbutt deriving (Eq, Show)
data Id a = MkId a deriving (Eq, Show)
data Product a b = Product a b deriving (Eq, Show)
data Sum a b = First a | Second b deriving (Eq, Show)
data RecordProduct a b = RecordProduct
                         { pfirst :: a
                         ,psecond::b} deriving (Eq, Show)

newtype NumCow = NumCow Int deriving (Eq, Show)
newtype NumPig = NumPig Int deriving (Eq, Show)
data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)
type Farmhouse' = Product NumCow NumPig


type Name = String
type Age = Int
type LovesMud = Bool
-- Sheep can produce between 2 and 30
-- pounds (0.9 and 13 kilos) of wool per year!
-- Icelandic sheep don't produce as much
-- wool per year as other breeds but the
-- wool they do produce is a finer wool.
type PoundsOfWool = Int
data CowInfo = CowInfo Name Age deriving (Eq, Show)
data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)
data SheepInfo = SheepInfo Name Age PoundsOfWool deriving (Eq, Show)
data Animal = Cow CowInfo
            | Pig PigInfo
            | Sheep SheepInfo deriving (Eq, Show)
-- Alternately
type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

data Animal2 = Sum CowInfo (Sum PigInfo SheepInfo)

trivialValue = Chickenbutt
