{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Lib
  where

import           Control.Applicative
import           Control.Monad
import           System.Random
import           Data.Maybe
import           Data.Monoid
import           Data.List

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
myLength' = foldr (const (+1)) 0

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
isPalindrome xs = xs == myReverse' xs

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

isPalindrome'' :: (Eq a) => [a] -> Bool
isPalindrome'' xs = fst $ fst $ foldr func ((True, xs), xs) xs
  where
    func _ ((r, as), []) = ((r, as), []) -- short circuit (even)
    func _ ((r, as), [d]) = ((r, as), [d]) -- short circuit (odd)
    func x ((r, a:as), _:_:ds) = ((r && (x == a), as), ds)

-- |7
data NestedList a = Elem a | List [NestedList a]
                  deriving (Show)

instance Functor NestedList where
    fmap f (Elem a) = Elem $ f a
    fmap f (List xs) = List $ map (fmap f) xs

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = foldr (flip (++) . flatten) [] xs


-- alternative type representation
flatten' :: NestedList a -> NestedList a
flatten' xs = List $ Elem <$> flatten xs

-- With GADT syntax:
-- type Func cxt a = cxt a => a -> a

data Tree a where
  Tree :: [Tree a] -> Tree a
  Leaf :: a ->  Tree a

-- treeShow :: Func Show a -> Func Show (Tree a)
-- treeShow = Tree

-- leafShow :: Func Show a -> Func Show (Leaf a)
-- leafShow = Leaf

instance Functor Tree where
  fmap f (Leaf x) = Leaf $ f x
  fmap f (Tree xs) = Tree $ map (fmap f) xs

flattenTree :: Tree a -> [a]
flattenTree (Leaf x) = [x]
flattenTree (Tree xs) = foldr ((++) . flattenTree) [] xs

flattenTree' :: (Show a) => Tree a -> Tree a
flattenTree' xs = Tree $ Leaf <$> flattenTree xs

-- |8

-- stack-open
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:[]) = [x]
compress (x₀:x₁:xs)
  | x₀ ==  x₁   = compress $ x₀:xs
  | otherwise = x₀:compress (x₁:xs)


-- foldr
compress' :: (Eq a) => [a] -> [a]
compress' [] = []
compress' (x:xs) = (:) x $ snd $ foldr fn (x, []) xs
  where
    fn :: (Eq a) => a -> (a, [a]) -> (a, [a])
    fn x (y, ys)
      | x == y    = (y, ys)
      | otherwise = (x, x:ys)


-- TODO: Make tail recursive. Perhaps examine how foldr works?
compress'' :: (Eq a) => [a] -> [a]
compress'' [] = []
compress'' (x:xs) = (:) x $ fn xs x
  where
    fn []         _   = []
    fn (new:rest) old
      | new == old = fn rest old
      | otherwise  = new : fn rest new

-- TODO: fix into an actual foldr. This is a foldl with leftwise accumulation function
-- myFoldr :: (a -> b -> b) -> b -> [a] -> b
-- myFoldr func = f
--   where
--     f y []     = y
--     f y (x:xs) = f (func x y) xs

-- |9
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = foldr fn [[x]] xs
  where
    fn :: (Eq a) => a -> [[a]] -> [[a]]
    fn x ((y:xs):xss)
      | x == y    = (x:x:xs):xss
      | otherwise = [y]:(x:xs):xss

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

encodeDirect :: (Eq a) => [a] -> [Enc a]
encodeDirect [] = []
encodeDirect (x:xs) = foldr fn [Single x] xs
  where
    fn :: (Eq a) => a -> [Enc a] -> [Enc a]
    fn x (Single y:ys)
      | x == y = Multiple 2 y : ys
      | otherwise = Single x : Single y : ys
    fn x (Multiple n y:ys)
      | x == y    = Multiple (n+1) y : ys
      | otherwise = Single x : Multiple n y : ys

-- |14
dupli :: [a] -> [a]
dupli = foldr ((++) . replicate 2) []

-- |15
repli :: [a] -> Int -> [a]
repli xs n = foldr ((++) . (replicate n)) [] xs -- point-free gets ugly when you need to flip

-- |16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = fst $ foldr fn ([], n) xs
  where
    fn x (xs, 0) = (xs, n)
    fn x (xs, i) = (x:xs, i - 1)

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
rotate xs n = drop n xs ++ take n xs

-- |20
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs  = ( xs!!(n-1)
                 , take (n-1) xs ++ drop n xs
                 )



-- |21
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = fn $ splitAt (n-1) xs
  where
    fn (a, b) = a ++ (x:b)

-- |22
range :: Int -> Int -> [Int]
range n m
  | n == m +1 = []
  | otherwise = n:range (n+1) m

range' :: Int -> Int -> [Int]
range' n m = [n..m]

range'' :: Int -> Int -> [Int]
range'' n m = take (m-n + 1) $ iterate (+1) n


-- TODO: get different seeds

-- |23
rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n = do
  g <- getStdGen
  return $ map (xs!!) $ take n $ randomRs (0, length xs - 1) g

-- |24
diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = do
  g <- getStdGen
  return $ take n $ randomRs (0, m) g

-- |25

rndPermu :: [a] -> IO [a]
rndPermu xs = rndSelect xs $ length xs


-- TODO: fix
-- |26

-- combinations :: [a] -> [[a]]
-- combinations xs = comb xs [] []
--   where
--     comb :: [a] -> [a] -> [a] -> [[a]]
--     comb (x:xs) keep toss
--       = comb xs (x:keep) toss
--       ++ comb xs keep (x:toss)
--     comb [] keep toss = [keep]

permu :: Int -> [[Int]]
permu n = (!!n) $ iterate (permu' n) [[]]

permu' :: Int -> [[Int]] -> [[Int]]
permu' n xs = catMaybes $ fn <$> [1..n]  <*> xs
  where
    fn x y
      | x `elem` y = Nothing
      | otherwise  = Just (x:y)

queens :: Int -> [[Int]]
queens n = filter testQueens $ permutations [1..n]

testQueens :: [Int] -> Bool
testQueens [x] = True
testQueens (x:xs) = and (zipWith fn [1..] xs) && testQueens xs
  where
    fn a y = x + a /= y && x - a /= y

printQueens :: [Int] -> String
printQueens xs
  = join . join $
  (\x -> "\n": take (x -1) spaces <> ("()": take (l - x) spaces))
  <$> xs
  where
    spaces = const "__" <$> [1..]
    l = length xs
