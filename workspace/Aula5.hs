module Aula5 where

import Data.Monoid
data Curso = SI | ADS | LOG | GP | GE deriving Eq

data Sacola = Sacola Int Int deriving Show

data Bolsa a = Bolsa a a deriving Show

data Mochila a b = Mochila a b deriving Show



class SimNao a where 
    simnao :: a -> Bool

instance SimNao Bool where 
    simnao True = True
    simnao False = False
    
instance SimNao Char where 
    simnao ' ' = False
    simnao _   = True
    

-- Show eh um typeclass, entao, ele terá uma funcao
-- a ser implementada para cada tipo. Ou seja,
-- Show Int -> Como mostrar na tela numeros inteiros
-- Show Curso -> Como na tela valores do tipo curso
instance Show Curso where 
    show SI = "Curso de design"
    show ADS = "Curso de COBOL + direito"
    show LOG = "Curso de caminhao"
    show GP  = "Curso de navio"
    show GE  = "Curso de ..."

msg :: (Show a, SimNao b) => a -> b -> String
msg a b = if (simnao b) then 
        ("Verdadeiro " ++ show a) 
    else 
        ("Falso " ++ show a)
    



data And = And Bool deriving Show

data Or = Or Bool deriving Show 

instance Monoid Or where 
    mempty = Or False
    mappend (Or x) (Or y) = Or (x || y)

instance Monoid And where 
    mempty = And True
    mappend (And x) (And y) = And (x && y)
