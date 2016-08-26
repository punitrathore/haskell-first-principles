module Programmer where

data OperatingSystem = GnuPlusLinux
                     | OpenBSDPlusNevermindJustBSDStill
                     | Mac
                     | Windows
                     deriving (Eq, Show)
data ProgrammingLanguage = Haskell
                         | Agda
                         | Idris
                         | PureScript deriving (Eq, Show)

data Programmer = Programmer { os :: OperatingSystem
                             , lang :: ProgrammingLanguage } deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [ GnuPlusLinux
                      , OpenBSDPlusNevermindJustBSDStill , Mac
                      , Windows
                      ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript, Haskell]

allProgrammers :: [Programmer]
allProgrammers = [(Programmer os l) | l <- allLanguages, os <- allOperatingSystems]


data Car = Car { make::String,
                 model::String,
                 year::Integer}
           deriving (Show, Eq)

data Automobile = Null
                | Automobile Car
                deriving (Show, Eq)

getYear :: Automobile -> Integer
getYear (Automobile (Car _ _ year)) = year

data Quantum = Yes
             |No
             | Both
             deriving (Eq, Show)

convert1 :: Quantum -> Bool
convert1 Yes = True
convert1 No = True
convert1 Both = True

convert2 :: Quantum -> Bool
convert2 Yes = True
convert2 No = True
convert2 Both = False

convert3 :: Quantum -> Bool
convert3 Yes = True
convert3 No = False
convert3 Both = True

convert4 :: Quantum -> Bool
convert4 Yes = True
convert4 No = False
convert4 Both = False

convert5 :: Quantum -> Bool
convert5 Yes = False
convert5 No = True
convert5 Both = True

convert6 :: Quantum -> Bool
convert6 Yes = False
convert6 No = True
convert6 Both = False

convert7 :: Quantum -> Bool
convert7 Yes = False
convert7 No = False
convert7 Both = True

convert8 :: Quantum -> Bool
convert8 Yes = False
convert8 No = False
convert8 Both = False

data Quad = One
          | Two
          | Three
          | Four
          deriving (Eq, Show)

                   

data Product a b = a:+:b
                 deriving (Eq, Show)


data List a = Nil | Cons a (List a) deriving (Show, Eq)

data BinaryTree a = Node (BinaryTree a) a (BinaryTree a)
                  | Leaf
                  deriving (Eq, Show)

tree :: BinaryTree Integer
tree = Node (Node (Leaf) 5 (Leaf)) 10 (Node Leaf 15 Leaf)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' n Leaf = Node Leaf n Leaf  
insert' n (Node lt n' rt) 
  | n > n' = (Node lt n' (insert' n rt))
  | n < n' = (Node (insert' n lt) n' rt)
  | n == n' = (Node lt n rt)

mapTree:: (a -> b) -> (BinaryTree a) -> (BinaryTree b)
mapTree _ Leaf = Leaf
mapTree f (Node lt a rt) = Node (mapTree f lt) (f a) (mapTree f rt)

inorder :: (BinaryTree a) -> [a]
inorder (Node Leaf n Leaf) = [n]
inorder (Node lt n rt) = (inorder lt) ++ [n] ++ (inorder rt)
inorder Leaf = []

preorder :: (BinaryTree a) -> [a]
preorder Leaf = []
preorder (Node lt n rt) = [n] ++ (preorder lt) ++ (preorder rt)

postorder :: (BinaryTree a) -> [a]
postorder Leaf = []
postorder (Node lt n rt) = (postorder lt) ++ (postorder rt) ++ [n]

foldTree :: (a -> b -> b -> b) -> b -> (BinaryTree a) -> b
-- foldTree f init tree = foldr f init (inorder tree)

foldTree _ init Leaf = init
foldTree f init (Node lt n rt) = f n (foldTree f init lt) (foldTree f init rt)


mapTree':: (a -> b) -> (BinaryTree a) -> (BinaryTree b)
mapTree' f t = foldTree (\n l r -> (Node l (f n) r) ) Leaf t

-- mapTree' :: (a -> b) -> BinaryTree a -> BinaryTree b
-- mapTree' f = foldTree (\a l r -> Node l (f a) r) Leaf