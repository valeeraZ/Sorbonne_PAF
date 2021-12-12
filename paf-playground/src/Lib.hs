module Lib
( someFunc
, maxInt
, fibo
) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

maxInt :: Integer -> Integer -> Integer 
maxInt a b = if a > b then a else b

-- >>> maxInt 13 10
-- 13

fibo :: Integer -> Integer 
fibo 0 = 1
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

-- >>> fibo 5
-- 8

fiboAux :: Integer -> Integer -> Integer -> Integer 
fiboAux n a b 
        | n == 0 = a
        | otherwise = fiboAux (n-1) b (a+b)

fiboTR :: Integer -> Integer 
fiboTR n = fiboAux n 1 1 

-- >>> fiboTR 5
-- 8

