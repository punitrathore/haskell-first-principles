module Jammin where

import Data.List

data Fruit = Peach | Plum | Apple | Blackberry deriving (Show, Eq, Ord)


-- data JamJars = Jam Fruit Int deriving(Show, Eq)

data JamJars = Jam { fruit:: Fruit,
                     count:: Int} deriving(Eq, Show, Ord)
               
row1 = Jam Peach 2
row2 = Jam Plum 3
row3 = Jam Apple 5
row4 = Jam Blackberry 10
row5 = Jam Peach 8
row6 = Jam Apple 8
allJam = [row1, row2, row3, row4, row5, row6]

totalJars = sum . (map count)

compareKind (Jam k _) (Jam k' _) = compare k k'

sortedJams = sortBy compareKind

groupKind (Jam k _) (Jam k' _) = k == k'
groupJam = groupBy groupKind $ sortedJams allJam