sayHello :: String -> IO()
sayHello x = putStrLn("Hello, " ++ x ++ "!")

triple :: Integer -> Integer
triple x = x * 3

-- areaOfCircle :: Integer -> Double
areaOfCircle x = 3.14 * x * x
