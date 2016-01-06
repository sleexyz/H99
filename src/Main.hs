module Main where

greet name = "Hello " ++ name ++ "!"

main :: IO ()
main = do
  putStrLn $ greet "World"
