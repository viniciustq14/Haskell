module Aula2 where

data Dia = Domingo | Segunda | Terca |
            Quarta | Quinta| Sexta |
            Sabado deriving (Show,Eq,Enum,Read)
data Day = Sunday |Monday|Tuesday |
            Wednesday |Thursday |Friday |
            Saturday  deriving (Show,Eq)

data Curso = ADS | SI | LOG 
           | GP  | GE deriving Show

data Aluno = Aluno {
    nome :: String, 
    curso :: Curso,
    salario :: Double 
} deriving Show

--aSalario :: Aluno -> Aluno
--aSalario Aluno(n c s)=Aluno n  c (1.2*s)

--bSalario :: Aluno -> Aluno 
--bSalario a = Aluno (nome a) (curso a) (1.2*(salario a))

agenda :: Dia -> String
agenda Domingo ="estudar"
agenda Segunda ="trabalhar"
agenda Quarta ="futebol"
agenda Sexta ="maldade"
agenda Sabado ="balada"
agenda _      ="bosta"

toDay :: Int->Either String  Dia
toDay 1 = Right Domingo
toDay 2 = Right Segunda
toDay 3 = Right Terca
toDay 4 = Right Quarta
toDay 5 = Right Quinta
toDay 6 = Right Sexta
toDay 7 =Right Sabado
toDay _ =Left "Dia errado"

traduzir :: Day ->  Either String Dia
traduzir Sunday =Right Domingo
traduzir Monday = Right Segunda
traduzir Tuesday=Right Terca
traduzir Wednesday=Right Quarta
traduzir Thursday=Right Quinta
traduzir Friday=Right Sexta
traduzir Saturday=Right Sabado
traduzir _ = Left "Nao ha traducao" 

data Unidade = KmH | MpH deriving Show

data Velocidade = Velocidade {
                valor :: Double ,
                unidade ::Unidade
                } deriving Show

converter :: Velocidade -> Velocidade
converter (Velocidade v KmH)=Velocidade (1.6*v) MpH
converter (Velocidade v MpH) =Velocidade (0.625*v) KmH




