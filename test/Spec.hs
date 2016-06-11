module Main
  where

import Lib
import Test.QuickCheck as Q


qc = Q.quickCheck


mkTest :: String -> IO () -> IO ()
mkTest desc test = putStrLn ("\n+++ Testing " ++ desc) >> test



-- TODO: Arbitrary samelength vectors

testTABA = let
    deconvolve :: [(a, b)] -> ([a], [b])
    deconvolve = (\(a, b) -> (a, reverse b)) . unzip

    naiveConvolve :: [a] -> [b] -> [(a, b)]
    naiveConvolve xs ys = zipWith (\a b -> (a, b)) xs (reverse ys)
  in
    do
      -- TODO: fix convolve
      mkTest "convolve (naive)" $ qc $
        \xs -> (deconvolve $ naiveConvolve xs xs) == (xs, xs) 

      mkTest "convolve (TABA tail rec)" $ qc $
        \xs -> (deconvolve $ convolve xs xs) == (xs, xs)

main = do

  mkTest "myLast (tail rec)" $ qc $
    \x -> not (null x) ==> last x == myLast x

  mkTest "myLast' (fold)" $ qc $
    \x -> not (null x) ==> last x == myLast' x

  mkTest "elementAt (tail rec)" $ qc $
    \xs i -> i > 0 && i < length xs ==> xs!!i == elementAt xs i

  mkTest "ting myReverse (tail rec)" $ qc $ 
    \xs -> (myReverse . myReverse) xs == xs

  mkTest "myReverse' (fold)" $ qc $ 
    \xs -> (myReverse' . myReverse') xs == xs

  mkTest "isPalindrome (naive)" $ qc $
    \xs -> isPalindrome xs == (reverse xs == xs)

  testTABA


