module Aula1 where

dobro :: Double -> Double 
dobro x = 2*x

somar :: Int -> Int -> Int
somar x y = x+y

u :: Int
u = 7

x :: Int
x = x+1

tamanho :: [Char] -> Int
tamanho xs = 1 + length xs

pin :: Int -> String
pin n 
    | mod n 4 == 0 = "PIN"
    | otherwise = show n