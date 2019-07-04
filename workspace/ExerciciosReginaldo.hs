module Exercicio1 where

-- 2.1 Gere	as	listas:

-- 2.1 A) [1,11,121,1331,14641,161051,1771561]

lista21a :: [Int]
lista21a = [11^x | x <- [0 .. 6]]



-- 2.1 B) [1,2,3,5,6,7,9,10,11,13,14,15,17,18,19,21,22,23, 25,26,27,29,
--     30,31,33,34,35,37,38,39]

lista21b :: [Int]
lista21b = [x | x <- [0 .. 40], mod x 4 /= 0]




-- 2.1 C) ["AaBB", "AbBB", "AcBB", "AdBB", "AeBB", "AfBB", "AgBB"]

lista21c :: [String]
lista21c = [x ++ y ++ z | x <- ["A"], y <- ["a","b", "c", "d", "e", "f", "g"], z <- ["BB"] ]




-- 2.1 D) [5,8,11,17,20,26,29,32,38,41]	

lista21d :: [Int]
lista21d  = [5 + (x-1)*3 | x <- [1 .. 13], x /= 4, x /= 7, x /= 12]




-- 2.1 E) [1.0,0.5,0.25,0.125,0.0625,0.03125]	

lista21e :: [Double]
lista21e  = [ x / y | x <- [1.0], y <- [1.0, 2.0, 4.0, 8.0, 16.0, 32.0] ]





-- 2.1 F) [1,10,19,28,37,46,55,64]	

lista21f :: [Int]
lista21f  = [1 + (x-1)*9 | x <- [1 .. 8] ]




-- 2.1 G) [2, 4, 6, 8, 10, 12, 16, 18, 22, 24, 28, 30]

lista21g :: [Int]
lista21g = [ x + 2 | x <- [0 .. 28], mod x 2 == 0, x /= 12, x /= 18, x /= 24]




-- 2.1 H) ['@','A','C','D','E','G','J','L']	

lista21h :: [Char]
lista21h  = [  x | x <- ['@' .. 'L'], x /= 'B', x /= 'F', x /= 'H', x /= 'I', x /= 'K' ]




-- 2.2 Crie uma função	que	verifique se o tamanho de uma String é par ou não.
--      Use Bool como retorno.

stringPar :: String -> Bool
stringPar x 
          | mod (length x) 2 == 0 = True
          | otherwise = False
          
          
          
          
          
-- 2.3  Escreva	uma	função que receba um vetor de Strings e
--      retorne uma lista com todos os elementos em	ordem reversa.

vetorReverso :: [String] -> [String]
vetorReverso xs = [ (reverse x) | x <- xs]





-- 2.4 Escreva uma função que receba um	vetor de Strings e
--     retorne uma lista com o tamanho de cada String.
--     As palavras de tamanho par devem	ser	excluídas da resposta. 

vetorTamanho :: [String] -> [Int]
vetorTamanho xs = [ (length x) | x <- xs, mod (length x) 2 /= 0 ]




-- 2.5 Escreva a função head como composição de duas outras.

funcaoHead :: String -> Char
funcaoHead xs = (last . reverse) xs



-- 2.6 Faça	uma	função que receba uma String e retorne True se esta for um palíndromo;
--     caso contrário, False. 

palindromo :: [Char] ->  Bool
palindromo x
           |(reverse x) == x = True
           | otherwise = False

--Função para tirar os espaços da frase.
tirarEspacos :: [Char] -> [Char]
tirarEspacos xs = [ x | x <- xs, x /= ' '] 





-- 2.7 Faça uma função que receba um inteiro e retorne uma tupla,
--     contendo: o dobro deste número na primeira coordenada,
--               o triplo na segunda, o	quádruplo na terceira e	o
--               quíntuplo na quarta.

funcaoTupla :: Int -> (Int, Int, Int, Int)
funcaoTupla xs = ( 2*xs, 3*xs, 4*xs, 5*xs )



-- ============================================================================

-- 3.1 	Crie o tipo Pergunta com os values constructors Sim	ou Nao.
--      Faça as funções	seguintes, determinando seus tipos explicitamente.

data Pergunta =  Sim | Nao deriving (Show)

---------------------------------
-- Exemplos:
---------------------------------
-- *Exercicios>  :t Sim
--               Sim :: Pergunta
---------------------------------
-- *Exercicios>  :t Nao
--               Nao :: Pergunta
---------------------------------


-- 3.1 A)
-- pergNum : recebe	via	parâmetro uma Pergunta. Retorna	0 para Nao e 1 para Sim.
            
pergNum :: Pergunta -> Int
pergNum Nao =  0
pergNum Sim =  1

------------------------------
-- Exemplos:
------------------------------
-- *Exercicios>   pergNum Sim
--                1
------------------------------
-- *Exercicios>   pergNum Nao
--                0
------------------------------



-- 3.1 B) listPergs : recebe via parâmetro uma lista de	Perguntas , e retorna 
--        0 n e 1 s correspondentes aos constructores contidos na lista.

listPergs :: [Pergunta] -> [Int]
listPergs xs = [ pergNum x |  x <- xs]

-- Obs. Usamos a função pergNum do exercicio 3.1 A)

-------------------------------------------------------------------------
-- Exemplos:
-------------------------------------------------------------------------
--  *Exercicios>   listPergs [Sim, Sim, Nao, Nao, Sim, Nao, Nao]
--                           [ 1 ,  1 ,  0 ,  0 ,  1 ,  0 ,  0 ]
-------------------------------------------------------------------------
--  *Exercicios>   listPergs [Sim, Sim, Nao, Nao, Sim, Nao, Na]
--  <interactive>:301:42:
--                         Not in scope: data constructor `Na'
--                         Perhaps you meant `Nao' (line 140)
-------------------------------------------------------------------------
-- *Exercicios>  listPergs [Sim, Sim, Nao, Nao, Sim, Nao, Sim, Sim, Sim]
--                         [1,1,0,0,1,0,1,1,1]
-------------------------------------------------------------------------




-- 3.1 C) and': recebe duas Perguntas como parâmetro e retorna a tabela
--        verdade do and lógico, usando Sim como verdadeiro e Nao como falso.

and' :: (Pergunta, Pergunta) -> Bool
and' (Nao, Nao) = False
and' (Nao, Sim) = False
and' (Sim, Nao) = False
and' (Sim, Sim) = True

---------------------------------
-- Exemplos:
---------------------------------
-- *Exercicios>  and' (Nao, Nao)
--                      False
---------------------------------
-- *Exercicios>  and' (Nao, Sim)
--                      False
---------------------------------
-- *Exercicios>  and' (Sim, Nao)
--                      False
---------------------------------
-- *Exercicios>  and' (Sim, Sim)
--                       True 
---------------------------------                         



-- 3.1 D) or': idem ao anterior, porém deve ser usado o ou lógico.

or' :: (Pergunta, Pergunta) -> Bool
or' (Nao, Nao) = False
or' (Nao, Sim) = True
or' (Sim, Nao) = True
or' (Sim, Sim) = True

---------------------------------
-- Exemplos:
---------------------------------
-- *Exercicios>  or' (Nao, Nao)
--                      False
---------------------------------
-- *Exercicios>  or' (Nao, Sim)
--                      True
---------------------------------
-- *Exercicios>  or' (Sim, Nao)
--                      True
---------------------------------
-- *Exercicios>  or' (Sim, Sim)
--                      True 
---------------------------------  


-- 3.1 E) not': idem aos anteriores, porém usando o not lógico.

not' :: (Pergunta, Pergunta) -> Bool
not' (Nao, Nao) = True
not' (Nao, Sim) = True
not' (Sim, Nao) = False
not' (Sim, Sim) = False

---------------------------------
-- Exemplos:
---------------------------------
-- *Exercicios>  not' (Nao, Nao)
--                       True
---------------------------------
-- *Exercicios>  not' (Nao, Sim)
--                       True
---------------------------------
-- *Exercicios>  not' (Sim, Nao)
--                      False
---------------------------------
-- *Exercicios>  not' (Sim, Sim)
--                      False 
---------------------------------


-- 3.2 Faça o tipo Temperatura que pode ter valores Celsius, Farenheit ou
--     Kelvin. Implemente as funções:

data Temperatura = Celsius| Farenheit | Kelvin deriving Show

data Unidades = K | C | F deriving Show

------------------------------------------
-- Exemplo:
------------------------------------------
-- *Exercicios> :t Celsius
--               Celsius :: Temperatura
------------------------------------------


-- 3.2 A) converterCelsius : recebe um valor double e uma temperatura,
--        e faz a conversão para Celsius

converterCelsius :: (Double, Temperatura) -> (Double, Unidades)
converterCelsius (x, Farenheit) = ( ((x-32)/1.8), C )
converterCelsius (x, Kelvin) = ( (x-273.15), C )
converterCelsius (x, Celsius) = ( x, C )

------------------------------------------------------------
-- Exemplos:
------------------------------------------------------------
-- *Exercicios> converterCelsius (270, Farenheit)
--              132.22222222222223
------------------------------------------------------------
-- *Exercicios> converterCelsius (212, Farenheit)
--              100.0
------------------------------------------------------------
-- *Exercicios> converterCelsius (0, Kelvin)
--              -273.15
------------------------------------------------------------
-- *Exercicios> converterCelsius (373.15, Kelvin)
--              100.0
------------------------------------------------------------
-- *Exercicios> converterCelsius (60, Celsius)
--              60.0     
------------------------------------------------------------



-- 3.2 B) converterKelvin: recebe um valor 	double 	e uma temperatura, 
--        e faz	a conversão para Kelvin.

converterKelvin :: (Double, Temperatura) -> (Double, Unidades)
converterKelvin (x, Celsius) = ( (x + 273.15), K )
converterKelvin (x, Farenheit) = (  ( (x-32)*(5/9) + 273.15 ), K  )
converterKelvin (x, Kelvin) = ( x, K )


------------------------------------------------
-- Exenplos:
------------------------------------------------
-- *Exercicios> converterKelvin (100, Celsius)
--                              (373.15,K)
------------------------------------------------
-- *Exercicios> converterKelvin (100, Farenheit)
--                        (310.92777777777775,K)
------------------------------------------------
-- *Exercicios> converterKelvin (100, Kelvin)
--                              (100.0,K)
------------------------------------------------



-- 3.2 C) converterFarenheit: recebe um valor double e uma temperatura,
--        e faz a conversão para Farenheit.


converterFarenheit :: (Double, Temperatura) -> (Double, Unidades)
converterFarenheit (x, Celsius) = (  ( ( x * (9/5) ) + 32 ) , F  )
converterFarenheit (x, Kelvin) = ( ( ( (x - 273.15) * (9/5) ) + 32 ), F )
converterFarenheit (x, Farenheit) = ( x, F )


-----------------------------------------------------------
--Exemplos:
-----------------------------------------------------------
-- *Exercicios>  converterFarenheit (100, Celsius)
--                                  (212.0,F)
-----------------------------------------------------------
-- *Exercicios>  converterFarenheit (100, Kelvin)
--                                  (-279.66999999999996,F)
-----------------------------------------------------------
-- *Exercicios>  converterFarenheit (100, Farenheit)
--                                  (100.0,F)
-----------------------------------------------------------


-- 3.3) Implemente uma função que simule o vencedor de uma partida de pedra,
--      papel e tesoura usando tipos criados. Casos de empate devem	ser
--      considerados em seu tipo. 

data Jokempo = Pedra | Papel | Tesoura deriving Show

--Obs.: Função Começar com mletra minuscula.

partida :: (Jokempo, Jokempo) -> String
partida (Pedra, Pedra) = "Empate"
partida (Pedra, Tesoura) = "A Pedra Ganhou - Quebrou a Tesoura!!"
partida (Pedra, Papel) = "O Papel Ganhou - Embrulhou a Pedra!!"

partida (Papel, Papel) = "Empate"
partida (Papel, Tesoura) = "A Tesoura Ganhou - Cortou o Papel"
partida (Papel, Pedra) =  "O Papel Ganhou - Embrulhou a Pedra!!"

partida (Tesoura, Tesoura) = "Empate"
partida (Tesoura, Papel) = "A Tesoura Ganhou - Cortou o Papel"
partida (Tesoura, Pedra) = "A Pedra Ganhou - Quebrou a Tesoura!!"


-----------------------------------------------------------------
-- Exemplos:
-----------------------------------------------------------------
-- *Exercicios>   partida (Pedra, Pedra)
--                        "Empate"
-----------------------------------------------------------------
-- *Exercicios>   partida (Pedra, Papel)
--                        "O Papel Ganhou - Embrulhou a Pedra!!"
-----------------------------------------------------------------
-- *Exercicios>   partida (Tesoura, Pedra)
--                        "A Pedra Ganhou - Quebrou a Tesoura!!"
-----------------------------------------------------------------
-- *Exercicios>   partida (Papel, Tesoura)
--                        "A Tesoura Ganhou - Cortou o Papel"
-----------------------------------------------------------------



-- 3.4) Faça uma função que retorne uma string, com	todas as vogais
--      maiúsculas e minúsculas eliminadas de uma string
--      passada por parâmetro usando list compreenshion. 


tiraVogais :: String -> String
tiraVogais xs = [ x | x <- xs, notElem x "AaEeIiOoUu"]

-------------------------------------------------------
-- Exemplo:
-------------------------------------------------------
-- *Exercicios>  tiraVogais "ABECabecIDidEFOGdfidUKu"
--                          "BCbcDdFGdfdK"
-------------------------------------------------------


tiraConsoantes :: String -> String
tiraConsoantes xs = [ x | x <- xs, elem x "AaEeIiOoUu"]

----------------------------------------------------------
-- Exxemplo:
----------------------------------------------------------
-- *Exercicios>  tiraConsoantes "ABECabecIDidEFOGdfidUKu"
--                              "AEaeIiEOiUu"
----------------------------------------------------------


eliminarVogaisMA :: String -> String
eliminarVogaisMA xs = [x | x <- xs,  x /= 'A', x /= 'E', x /= 'I', x /= 'O', x /= 'U']

eliminarVogaisMi :: String -> String
eliminarVogaisMi xs = [ x | x <- xs,  x /= 'a',  x /= 'e', x /= 'i', x /= 'o', x /= 'u']

eliminarVogais :: String -> String
eliminarVogais x = (eliminarVogaisMA . eliminarVogaisMi) x

-------------------------------------------------------------
-- Exemplos:
-------------------------------------------------------------
-- *Exercicios>   eliminarVogaisMA "AEIOUBaeioucAEIOUDaeioup"
--                                "BaeioucDaeioup"

-- Obs.: Função que elimina as vogais Maiúsculas
-------------------------------------------------------------
-- *Exercicios>    eliminarVogaisMi "AEIOUBaeioucAEIOUDaeiouf"
--                                 "AEIOUBcAEIOUDf"

-- Obs.: Função que elimina as vogais Minúsculas
-------------------------------------------------------------
-- *Exercicios>   eliminarVogais "AEIOUBaeioucAEIOUDaeioup"
--                               "BcDp"

-- Obs.: Função que elimina qualquer todas as vogais
-------------------------------------------------------------
-- *Exercicios> eliminarVogais "AEIOUKaeiourAEIOUL"
--                             "KrL"
-------------------------------------------------------------



retornarVogais :: String -> String
retornarVogais xs = [x | x <- xs,  x == 'a'|| x == 'e' ||  x == 'i' ||  x == 'o' ||  x == 'u'
                                || x == 'A'|| x == 'E' ||  x == 'I' ||  x == 'O' ||  x == 'U'] 

------------------------------------------------------------
-- Exempo:
------------------------------------------------------------
-- *Exercicios>  retornarVogais "ABECIDOFUabecidofu"
--                              "AEIOUaeiou"
------------------------------------------------------------


---------
-- Extra
---------
removeMinusculas :: String -> String
removeMinusculas st = [ c | c <- st, c `elem` ['A'..'Z']]
  
------------------------------------------------------------  
-- Exemplo: 
------------------------------------------------------------
-- *Exercicios>  removeMinusculas "AaBbCcDdEeFf"
--                                "ABCDEF"
------------------------------------------------------------



-- 3.5) Sabe-se que as unidades imperiais de comprimento podem ser
--      Inch, Yard ou Foot (há outras ignoradas aqui).
--      Sabe-se que 1in=0.0254m, 1yd=0.9144m, 1ft=0.3048.
--      Faça a função:
--      converterMetros que	recebe a unidade imperial e o valor correspondente
--      nesta unidade. Esta função deve retornar o valor em metros.
--      Implemente também a função converterImperial, que recebe um valor
--      em metros e a unidade de conversão. Esta função deve retornar o valor
--      convertido para a unidade desejada.



data UnidadesImperial = Inch | Yard | Foot deriving Show

converterMetros :: UnidadesImperial -> Double -> (Double, Char)
converterMetros Inch x  = ( x*0.0254, 'm')
converterMetros Yard x  = ( x*0.9144, 'm')
converterMetros Foot x  = ( x*0.3048, 'm')

converterImperial :: Double -> UnidadesImperial -> (Double, String)
converterImperial x Inch = (x*39.3701, "in")
converterImperial x Yard = (x*1.09361, "yd")
converterImperial x Foot = (x*3.28083, "ft")


-------------------------------------------
-- Exemplos:
-------------------------------------------
-- *Exercicios> converterMetros Inch 10
--                              (0.254,"m")
-------------------------------------------
-- *Exercicios> converterMetros Yard 10
--                              (9.144,"m")
-------------------------------------------
-- *Exercicios> converterMetros Foot 10
--                              (3.048,"m")
-------------------------------------------
-- *Exercicios> converterImperial 10 Inch
--                           (393.701,"in")
-------------------------------------------
-- *Exercicios> converterImperial 10 Yard
--                           (10.9361,"yd")
-------------------------------------------
-- *Exercicios> converterImperial 10 Foot
--                           (32.8083,"ft")
-------------------------------------------



-- 3.6) Faça um novo tipo chamado Mes, que possui como valores todos os meses
--      do ano. Implemente: A função checaFim , que retorna o número de dias
--      que cada mês possui (considere fevereiro tendo 28 dias).
--      A função prox, que recebe um mês atual e retorna o próximo mês.
--      A função estacao, que retorna a estação do ano de acordo com o mês
--      e com o hemisfério.
--      Use apenas tipos criados pela palavra data aqui.

data Mes = Jan| Fev | Mar | Abr | Mai | Jun | Jul | Ago | Set | Out | Nov | Dez
           deriving (Show, Enum)

data Estacao = Verao | Outono | Inverno | Primavera deriving Show

data Hemisferio = Norte | Sul deriving Show
           

checaFim :: Mes -> (Int, String)
checaFim Jan = (31, "Dias")
checaFim Fev = (28, "Dias")
checaFim Mar = (31, "Dias")
checaFim Abr = (30, "Dias")
checaFim Mai = (31, "Dias")
checaFim Jun = (30, "Dias")
checaFim Jul = (31, "Dias")
checaFim Ago = (31, "Dias")
checaFim Set = (30, "Dias")
checaFim Out = (31, "Dias")
checaFim Nov = (30, "Dias")
checaFim Dez = (31, "Dias")

proxMes :: Mes -> Mes
proxMes Jan = Dez
proxMes Fev = Mar
proxMes Mar = Abr
proxMes Abr = Mai
proxMes Mai = Jun
proxMes Jun = Jul
proxMes Jul = Ago
proxMes Ago = Set
proxMes Set = Out
proxMes Out = Nov
proxMes Nov = Dez
proxMes Dez = Jan

estacaoAno :: (Mes, Hemisferio) -> Estacao
estacaoAno (Jan, Norte) = Inverno
estacaoAno (Fev, Norte) = Inverno
estacaoAno (Mar, Norte) = Inverno
estacaoAno (Abr, Norte) = Primavera
estacaoAno (Mai, Norte) = Primavera
estacaoAno (Jun, Norte) = Primavera
estacaoAno (Jul, Norte) = Verao
estacaoAno (Ago, Norte) = Verao
estacaoAno (Set, Norte) = Verao
estacaoAno (Out, Norte) = Outono
estacaoAno (Nov, Norte) = Outono
estacaoAno (Dez, Norte) = Outono

estacaoAno (Jan, Sul) = Verao
estacaoAno (Fev, Sul) = Verao
estacaoAno (Mar, Sul) = Verao
estacaoAno (Abr, Sul) = Outono
estacaoAno (Mai, Sul) = Outono
estacaoAno (Jun, Sul) = Outono
estacaoAno (Jul, Sul) = Inverno
estacaoAno (Ago, Sul) = Inverno
estacaoAno (Set, Sul) = Inverno
estacaoAno (Out, Sul) = Primavera
estacaoAno (Nov, Sul) = Primavera
estacaoAno (Dez, Sul) = Primavera


---------------------------------------
--Exemplo:
---------------------------------------
-- *Exercicios> checaFim Fev
--              (28,"Dias")
---------------------------------------
-- *Exercicios> proxMes Jan
--              Dez
---------------------------------------
-- *Exercicios> proxMes Mar
--              Abr
--------------------------------------
-- *Exercicios> estacaoAno (Jan, Sul)
--                         Verao
---------------------------------------
-- *Exercicios> estacaoAno (Jan, Norte)
--                         Inverno
---------------------------------------




-- 3.7) Faça uma função que  receba uma String e retorne True se esta
--      for um palíndromo; caso contrário, False.


palindromo2 :: String ->  Bool
palindromo2 x
            |(reverse x) == x = True
            | otherwise = False

-- Obs.: Este exercício é igual ao 2.6







-- 3.10) Faça uma função chamada revNum, que receba uma String s e
--       um Int n. Esta deverá retornar as n primeiras letras em ordem reversa
--       e o restante em sua ordem normal.
-------------------------------------------------------------------------------
--       Exemplo:
--       revNum	4	"FATEC"	=	"ETAFC"
-------------------------------------------------------------------------------


revNum :: String -> Int -> String
revNum s n =  reverse [  x  | x <- take n s] ++ [ y | y <- drop n s]

---------------------------------------
-- Exemplos:
---------------------------------------
-- *Exercicios>  revNum "123456" 3
--               "321456"
---------------------------------------
-- * Exercicios>  revNum "FATEC" 3
--                      "TAFEC"
---------------------------------------
-- *Exercicios>  revNum "Ola Mundo" 3
--               "alO Mundo"
---------------------------------------




-- 3.13) Faça um novo tipo chamado Metros, que possui um
--       \textit{value constructor} de mesmo nome, cujos parâmetros são:
--       um Int que representa a dimensão, e um Double que representa
--       o valor da medida e outro chamado MetragemInvalida.
--       Implemente as funções:

--     	areaQuadrado :: Metros -> Metros: calcula a área de um quadrado.
--      areaRet :: Metros -> Metros -> Metros: calcula a área de um retângulo.
--      areaCubo :: Metros -> Metros: calcula a área de um cubo.

--      Use o pattern matching para ignorar as metragens erradas 
--      (calcular a área de um quadrado com um lado de dimensão 4 não é válido).


---------------------------------------------------
-- Exemplo:
---------------------------------------------------
--  Prelude>  areaQuadrado (Metros 1 2.0)
--                          Metros 2 4.0
---------------------------------------------------

data MetragemInvalida = MetragemInvalida {msg :: String} deriving Show

data Metros = Metros { dimensao::Int, medida :: Double } deriving Show



areaQuadrado :: Metros -> Either MetragemInvalida Metros
areaQuadrado (Metros 1 m) = Right (Metros 2 (m^2))  
areaQuadrado (Metros _ m) = Left (MetragemInvalida "Dimensao Errada, Tente Novamente")


------------------------------------------------------------------------------------
-- Exemplos:
------------------------------------------------------------------------------------
--  *Exercicios>  areaQuadrado (Metros 1 5)
--                Right (Metros {dimensao = 2, medida = 25.0})
------------------------------------------------------------------------------------
--  *Exercicios>  areaQuadrado (Metros 2 5)
--                Left (MetragemInvalida {msg = "Dimensao Errada, Tente Novamente"})
------------------------------------------------------------------------------------


areaRet ::Metros -> Metros -> Either MetragemInvalida Metros
areaRet (Metros 1 x) (Metros 2 y) = Right ( Metros 2 (x*y) )  
areaRet (Metros _ x) (Metros _ y) = Left (MetragemInvalida "Dimensao Errada, Tente Novamente")

------------------------------------------------------------------------------------
-- Exemplos:
------------------------------------------------------------------------------------
-- *Exercicios>  areaRet (Metros 1 2)(Metros 2 3)
--               Right (Metros {dimensao = 2, medida = 6.0})
------------------------------------------------------------------------------------
--  *Exercicios>  areaRet (Metros 1 2)(Metros 3 3)
--                Left (MetragemInvalida {msg = "Dimensao Errada, Tente Novamente"})
------------------------------------------------------------------------------------



-- O cubo é um poliedro regular pois as suas faces são geometricamente iguais.
-- São 6 faces, que são quadrados geometricamente iguais.

areaCubo :: Metros -> Either MetragemInvalida Metros
areaCubo (Metros 1 m) = Right ( Metros 3 ( 6*(m^2) )  )  
areaCubo (Metros _ m) = Left (MetragemInvalida "Dimensao Errada, Tente Novamente")

------------------------------------------------------------------------------------
-- Exemplos:
------------------------------------------------------------------------------------
-- *Exercicios> areaCubo (Metros 1 3)
--              Right (Metros {dimensao = 3, medida = 54.0})
------------------------------------------------------------------------------------
-- *Exercicios> areaCubo (Metros 1 5)
--              Right (Metros {dimensao = 3, medida = 150.0})
------------------------------------------------------------------------------------
-- *Exercicios> areaCubo (Metros 0 5)
--              Left (MetragemInvalida {msg = "Dimensao Errada, Tente Novamente"})
------------------------------------------------------------------------------------