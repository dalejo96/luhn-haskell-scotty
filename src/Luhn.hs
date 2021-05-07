module Luhn where

toDigits :: Integer -> [Integer]
toDigits x | x <= 0 = []
toDigits x =
  let (a, b) = x `divMod` 10
   in toDigits a ++ [b]

double :: Integer -> Integer
double x = x + x

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther numbersList@(x : y : xs)
  | odd (length numbersList) = x : double y : doubleEveryOther xs
  | otherwise = double x : y : doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
--sumDigits (x : xs) = sum (toDigits x) + sumDigits xs
sumDigits xs = foldr ((+) . sum . toDigits) 0 xs

validateLunh :: Integer -> Bool
validateLunh x = sumDigits (doubleEveryOther (toDigits x)) `mod` 10 == 0