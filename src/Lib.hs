module Lib
  where
import           Control.Applicative
import           Control.Monad
import           System.Random

-- |1

-- tail recursive method
myLast :: [a] -> a
myLast (x:[]) = x
myLast (x:xs) = myLast xs

-- fold method
myLast' :: [a] -> a
myLast' = foldl (const id) (error "List cannot be empty!")

-- |2

-- tail recursive method
myButLast :: [a] -> a
myButLast (x: _ :[]) = x
myButLast (_:xs) = myButLast xs


-- fold method
myButLast' :: [a] -> a
myButLast' = fst . foldl (\(_, r)  x -> (r, x)) (err, err)
  where
   err = error "List is too small!"

-- |3

-- tail recursive method
elementAt :: [a] -> Int -> a
elementAt (x:xs) 0 = x
elementAt (x:xs) i = elementAt xs (i - 1)

-- fold method
elementAt' :: [a] -> Int -> a
elementAt' xs index = fstButCheck $ foldl f (error "Index too small!", index) xs
                  where
                    f :: (a, Int) -> a -> (a, Int)
                    f (y, 0) _ = (y, 0)
                    f (_, n) x = (x, n - 1)

                    fstButCheck :: (a, Int) -> a
                    fstButCheck (x, n)
                      | n > 0 = error "index too big!"
                      | otherwise = x

-- |4

-- tail recursive method
myLength :: [a] -> Int
myLength xs = fn xs 0
              where
                fn [] i = i
                fn (x:xs) i = fn xs (i + 1)

-- fold method
myLength' :: [a] -> Int
myLength' = foldr ((+) . const 1) 0

-- |5

-- tail recursive method
myReverse :: [a] -> [a]
myReverse xs = fn xs []
               where
                 fn [] ys = ys
                 fn (x:xs) ys = fn xs (x:ys)


-- fold method
myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []


-- |6

-- naive method
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == foldl (flip (:)) [] xs

-- TABA method

-- First we demonstrate convolving two lists tail recursively
convolve :: [a] -> [b] -> [(a, b)]
convolve xs ys = fst . walk $ xs
  where
    walk [] = ([], ys)
    walk (a:as) =
      let
        (r, b:bs) = walk as
      in
        ((a, b):r, bs)

-- This convolution can be summarized in a foldr
convolve' :: [a] -> [b] -> [(a, b)]
convolve' xs ys = fst $ foldr pluckPlace ([], ys) xs
  where
    pluckPlace a (r, b:bs) = ((a, b):r, bs)

-- Now we convolve two halves.
-- We run two pointers, one ordinary TABA pointer at single time, and an extra pointer at double time.
-- When the double time reaches an empty or last element, we know that the TABA pointer is in the middle
-- In which case we spring back up.

convolveHalves :: [a] -> [(a, a)]
convolveHalves xs = fst $ walk xs xs
  where
    walk as [] = ([], as)
    walk (_:as) [_] = ([], as)
    walk (a:as) (_:_:ds) =
      let
        (r, b:bs) = walk as ds
      in ((a, b):r, bs)


-- This convolution is inexpressible in a foldr.
-- because implicity we've lost the double-time pointer

-- Here we switch out tuple-making with equality checking, and cons with &&
-- and we get isPalindrome!
isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' xs = fst $ walk xs xs
  where
    walk as [] = (True, as)
    walk (_:as) [_] = (True, as)
    walk (a:as) (_:_:ds) =
      let
        (r, b:bs) = walk as ds
      in (r && (a == b), bs)


-- |7
data NestedList a = Elem a | List [NestedList a]
                  deriving (Show)

instance Functor NestedList where
    fmap f (Elem a) = Elem $ f a
    fmap f (List xs) = List $ map (fmap f) xs

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = foldr (flip (++) . flatten) [] xs


-- alternative representations
flatten' :: NestedList a -> NestedList a
flatten' xs = List $ map Elem $ flatten xs


-- |8
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

-- |9
pack :: Eq a => [a] -> [[a]]
pack xs = reverse $ fn xs []
          where
            fn [] ys = ys
            fn (x:xs) [] = fn xs [[x]]
            fn (x:xs) ((y:ys):yss)
               | x == y = fn (xs) ((x:y:ys):yss)
               | otherwise = fn (xs) ([x]:(y:ys):yss)

-- |10
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\xs -> (length xs, head xs)) . pack

-- |11
data Enc a = Multiple Int a | Single a
           deriving (Show)

encodeModified xs = map f $ encode xs
                    where
                      f (1, x) = Single x
                      f (n, x) = Multiple n x

-- |12
decodeModified :: Eq a => [Enc a] -> [a]
decodeModified = foldr ((++) . fn) []
                 where
                   fn (Single a) = [a]
                   fn (Multiple n a) = replicate n a

-- |13

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

-- |14
dupli :: [a] -> [a]
dupli = foldr ((++) . (replicate 2)) []

-- |15
repli :: [a] -> Int -> [a]
repli xs = \n -> foldr ((++) . (replicate n)) [] xs -- point-free gets ugly when you need to flip

-- |16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = reverse $ fn xs [] n
                 where
                   fn [] ys _ = ys
                   fn (x:xs) ys 0 = fn xs ys n
                   fn (x:xs) ys i = fn xs (x:ys) (i-1)

-- |17
mySplit :: [a] -> Int -> ([a], [a])
mySplit xs n = fn xs [] n
           where
             fn xs ys 0 = (reverse ys, xs)
             fn (x:xs) ys n = fn xs (x:ys) (n-1)

-- |18
mySlice :: [a] -> Int -> Int -> [a]
mySlice xs n m = fst $ (flip mySplit) (m-n+1) $ snd $ mySplit xs (n-1)



-- |19
rotate :: [a] -> Int -> [a]
rotate xs n = fn xs [] (n `mod` length xs)
              where
                fn xs ys 0  = (xs) ++ (reverse ys)
                fn (x:xs) ys i = fn xs (x:ys) (i-1)

-- |20
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs  = ((xs!!(n-1)), (take (n-1) xs) ++ (drop n xs))


removeAt' :: Int -> [a] -> (a, [a])
removeAt' n xs  = fn xs [] (n-1)
  where
    fn (x:xs) ys 0 = (x, (reverse ys)++xs)
    fn (x:xs) ys i = fn xs (x:ys) (i-1)

-- |21
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = fn $ splitAt (n-1) xs
  where
    fn (a, b) = a ++ (x:b)

-- |22
range :: Int -> Int -> [Int]
range n m
      | n == m +1 = []
      | otherwise = n:(range (n+1) m)

range' :: Int -> Int -> [Int]
range' n m = [n..m]


-- |23
rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n = do
  pos <- replicateM n $
         getStdRandom $ randomR (0, (length xs) -1)
  return [xs !! x | x <- pos]

-- |24
