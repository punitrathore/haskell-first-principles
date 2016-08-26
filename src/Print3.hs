module Print3 where

myGreeting :: String
myGreeting = "hell" ++ "o world"

main :: IO()
main = do
  putStrLn myGreeting
  putStrLn secondGreeting
    where secondGreeting = concat["hellO"," ", "wOrld"]

area d = pi * (r * r)
  where r = d / 2  
