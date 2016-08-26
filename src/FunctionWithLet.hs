module FunctionWithLet where

printInc2 n = let plusTwo = n + 2
              in print plusTwo

printInc2' n = (\plusTwo -> print plusTwo)(n + 2)                 


b = x * 5 where x = 10 * 5 + y
                y = 10

-- let x = 7; y = negate x; z = y * 10 in z / x + y
c = z / x + y where x = 7
                    y = negate x
                    z = y * 10

inc x = x + 1

waxOn = x * 5 where z = 7
                    y = z + 8
                    x = y ^ 2

triple x = x * 3

waxOff x = triple $ (x*x) / 10
