module Main where

import Control.Monad
import Control.Applicative

-- 1
myLast :: [a] -> a
myLast (x:[]) = x
myLast (x:xs) = myLast xs

-- 2
myButLast :: [a] -> a
myButLast (x: _ :[]) = x
myButLast (_:xs) = myButLast xs

-- 3
elementAt :: [a] -> Int -> a
elementAt (x:xs) 0 = x
elementAt (x:xs) i = elementAt xs (i - 1)

-- 4
myLength :: [a] -> Int
myLength xs = fn xs 0
              where
                fn [] i = i
                fn (x:xs) i = fn xs (i + 1)

myLength' :: [a] -> Int
myLength' = foldr ((+) . (\_ -> 1)) 0

-- 5
myReverse :: [a] -> [a]
myReverse xs = fn xs []
               where
                 fn [] ys = ys
                 fn (x:xs) ys = fn xs (x:ys)


myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []

-- 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == myReverse xs

-- 7
data NestedList a = Elem a | List [NestedList a]
                  deriving (Show)

instance Functor NestedList where
    fmap f (Elem a) = Elem $ f a
    fmap f (List xs) = List $ map (fmap f) xs

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = foldr (flip (++) . flatten) [] xs


-- 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress xs = reverse $ fn xs []
             where
               fn [] ys = ys
               fn (x:[]) ys = (x:ys)
               fn (x1:x2:xs) ys
                  | x1 == x2 = fn (x2:xs) ys
                  | otherwise = fn (x2:xs) (x1:ys)

-- TODO: can I compress without reverse?

-- 9
pack :: Eq a => [a] -> [[a]]
pack xs = reverse $ fn xs []
          where
            fn [] ys = ys
            fn (x:xs) [] = fn xs [[x]]
            fn (x:xs) ((y:ys):yss)
               | x == y = fn (xs) ((x:y:ys):yss)
               | otherwise = fn (xs) ([x]:(y:ys):yss)

-- 10
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\xs -> (length xs, head xs)) . pack

-- 11
data Enc a = Multiple Int a | Single a
           deriving (Show)

encodeModified xs = map f $ encode xs
                    where
                      f (1, x) = Single x
                      f (n, x) = Multiple n x

-- 12
decodeModified :: Eq a => [Enc a] -> [a]
decodeModified = foldr ((++) . fn) []
                 where
                   fn (Single a) = [a]
                   fn (Multiple n a) = replicate n a

-- 13
-- Can I do this without reverse?
encodeDirect :: Eq a => [a] -> [Enc a]
encodeDirect xs = reverse $ fn xs []
                  where
                    fn [] ys = ys
                    fn (x:xs) [] = fn xs [(Single x)]
                    fn (x:xs) ((Single y):ys)
                       | x == y = fn (xs) ((Multiple 2 y):ys)
                       | otherwise = fn (xs) ((Single x):(Single y):ys)
                    fn (x:xs) ((Multiple n y):ys)
                       | x == y = fn (xs) ((Multiple (n+1) y):ys)
                       | otherwise = fn (xs) ((Single x):(Multiple n y):ys)

-- 14
dupli :: [a] -> [a]
dupli = foldr ((++) . (replicate 2)) []

-- 15
repli :: [a] -> Int -> [a]
repli xs = \n -> foldr ((++) . (replicate n)) [] xs -- point-free gets ugly when you need to flip

--16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = reverse $ fn xs [] n
                 where
                   fn [] ys _ = ys
                   fn (x:xs) ys 0 = fn xs ys n
                   fn (x:xs) ys i = fn xs (x:ys) (i-1)

-- 17
split :: [a] -> Int -> ([a], [a])
split xs n = fn xs [] n
           where
             fn xs ys 0 = (reverse ys, xs)
             fn (x:xs) ys n = fn xs (x:ys) (n-1)

-- 18
slice :: [a] -> Int -> Int -> [a]
slice xs n m = fst $ (flip split) (m-n+1) $ snd $ split xs (n-1)



-- 19
rotate :: [a] -> Int -> [a]
rotate xs n = fn xs [] (n `mod` length xs)
              where
                fn xs ys 0  = (xs) ++ (reverse ys)
                fn (x:xs) ys i = fn xs (x:ys) (i-1)

-- 20
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs  = ((xs!!(n-1)), (take (n-1) xs) ++ (drop n xs))


removeAt' :: Int -> [a] -> (a, [a])
removeAt' n xs  = fn xs [] (n-1)
                  where
                    fn (x:xs) ys 0 = (x, (reverse ys)++xs)
                    fn (x:xs) ys i = fn xs (x:ys) (i-1)






main :: IO ()
main = do
  putStrLn "Hello World!"
