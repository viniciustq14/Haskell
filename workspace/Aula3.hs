module Aula3 where

fat :: Integer->Integer
fat n 
    |n <= 1=1
    |otherwise = n * fat(n-1)

fib:: Integer-> Integer
fib 0=0
fib 1=1
fib n= fib(n-1)+ fib(n-2)

data Status = Saudavel | Gordo | Magro
    deriving Show

imc :: Double -> Double -> Status
imc peso altura 
    | z <= 18 = Magro
    | z <= 25 = Saudavel
    | otherwise = Gordo
    where 
        z = peso / (altura*altura)
        
elimVogal :: String -> String 
elimVogal [] = []
elimVogal (x:xs)
    | elem x "AEIOUaeiou" = elimVogal xs
    | otherwise = x : elimVogal xs

somar :: Int -> Int -> Int -> Int 
somar x y z = x+y+z        
