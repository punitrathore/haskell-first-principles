module TypeInference1 where

-- f x y=x+y+3

myConcat x = x ++ "yo"

myMult x = (x/3)*5

myCom x = x > (length [1..10])

myAlph x = x < 'z'

triple :: Int -> Int
triple x = x * 3


x=5
-- y=x+5
-- w=y*10

-- x=5
-- y=x+5
-- f=4/y

-- bigNum= (^)5 $ 10

-- a=(+)
-- b=5
-- -- c=b 10
-- -- d=c 200


-- a=12+b
-- b=10000*c

functionH (x:_) = x

functionC x y = if (x > y) then True else False

foo1 x y = y
foo2 x y = x


r x = tail x
