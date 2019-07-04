module Execicios where

import Data.Monoid
--3.10
revNum :: String -> Int ->String
revNum a b = (reverse $ take b a)++drop b a

--3.4
--eliminaVogais::[Char]->String
--eliminaVogais xs =[x |x<-xs,not $ elem x "aeiouAEIOU"]


elVg:: String->String
elVg xs=[x|x<-xs,v<-"bcdfghjklmnpqrstvxwyzBCDFGHJKLMNPQRSTVXWYZ",x==v]


--3.13
data Metro = Metro {
            dimensao::Int,
            medida :: Double
            }deriving Show

areaQuadrado::Metro->Either String Metro
areaQuadrado (Metro 1 m) =Right (Metro 2 (m^2))  
areaQuadrado (Metro _ m)=Left "Dimensao esta errada"

areaRetangulo:: Metro->Metro->Either String Metro
areaRetangulo (Metro 1 m) (Metro 1 a)= Right (Metro 2 (m*a))
areaRetangulo (Metro _ m) (Metro _ a)=Left "Dimensao esta errada"

areaCubo:: Metro ->Either String Metro
areaCubo (Metro 1 m) =Right (Metro 3 (m^3)) 
areaCubo (Metro _ m)=Left "Dimensao esta errada"


data TipoProduto = Escritorio | Informatica | Livro | Filme | Total deriving (Show, Eq)
data Produto = Produto {valor :: Double,
                            tp :: TipoProduto} | Nada deriving (Show, Eq)


instance Monoid Produto where
mempty = Nada
mappend (Produto valor1 tp1) (Produto valor2 tp2)  =Produto (valor1+valor2) Total

data Indicativo   = Credito | Debito  deriving (Show,Eq)
data ContaCorrente = ContaCorrente {valorC :: Double
                    ,indicativo::Indicativo } deriving(Show,Eq)
                    
maiorCredito::[ContaCorrente]->Double->ContaCorrente
maiorCredito cs vl=foldl (\ (ContaCorrente v ind) (ContaCorrente v2 ind2) -> 
            if (v > v2) then (ContaCorrente v ind) else (ContaCorrente v2 ind2)) (ContaCorrente 0.0 Credito) $
    filter (\ (ContaCorrente valor indicativo) -> indicativo == Credito && valor < vl ) cs

maiorMenorDebito :: [ContaCorrente] -> (ContaCorrente , ContaCorrente)
maiorMenorDebito listaContas =()
   


data Quad a = Quad String Bool a Int
