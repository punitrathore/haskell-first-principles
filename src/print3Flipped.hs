module Print3Flipped where

main :: IO()
main = do
  putStrLn foo
    where foo = ((++) "hello" ((++) " " "world"))

thirdLetter :: [Char] -> Char
thirdLetter x = x !! 2

rvrs s = (++) (drop 9 s) $ (++) (drop 5 (take 9 s)) (take 5 s) 

data Mood = Blah | Woot deriving Show


changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah
