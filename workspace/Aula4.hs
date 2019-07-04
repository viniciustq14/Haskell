module Aula4 where

--Exercicios do cap 4
--4.1
--calMedia::[Double]->Double
--calMedia xs = (foldl (+) 0 xs / (length xs)

--4.2
ehPalindromo::[String]->[String]
ehPalindromo ss=filter (\xs-> xs==reverse xs) ss

--4.3
ehPar ::[Int]->[Int]
ehPar xs= filter (\x -> even x) xs

ehImpar::[Int]->[Int]
ehImpar xs= filter (\x -> odd x) xs

--4.4
ehPrimo::[Int]->[Int]
ehPrimo xs = filter (\x -> if x % [1..x]/=0 then x ) xs