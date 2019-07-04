module 5.1 where 

import Data.Monoid


-- FAZER OS EXERCICIOS 5.1,5.2 E 5.5

-- 5.1 crie o tipo TipoProduto que possui os values constructors Escritorio, Informatica
--Livro, Filme e Total. O tipo Produto possui um value constructor - de mesmo
--nome - e os campos valor(Double), tp(TipoProduto) e um value constructor Nada
--que representa a ausencia de um produto. 
--Deseja-se Calcular o valor total
-- de uma compra, de modo a não ter nenhuma conversão para inteiro e de forma com
--binavel. Crie uma instancia de monoide para Produto, de modo que o retorno sempre
--tenha Total no campo tp e a soma dos dois produtos em valor. Explique como seria
--o exercicio sem o uso das monoides. Qual seriam as diferenças?

data TipoProduto = Escritorio | Informatica | Livro | Filme | Total deriving (Show, Eq)
data Produto = Produto {valor :: Double,
                            tp :: TipoProduto} | Nada deriving (Show, Eq)

instance Monoid Produto where
    mempty = Nada
    mappend Escritorio Informatica Livro Filme = Total
    mappend (Produto valor) (Total tp)  = (Produto (valor+valor)) (Total tp++tp)
