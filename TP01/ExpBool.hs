module ExpBool where

import Control.Monad(replicateM)
import Data.List

data ExpBool = Cte Bool             --constantes
         | Var String               --variaveis
         | Neg ExpBool              --negacao
         | Con ExpBool ExpBool      --conjuncao (e)
         | Dis ExpBool ExpBool      --disjuncao (ou)
         deriving (Show)

type Memoria = [(String,Bool)]
type TabelaVerdade = [(Memoria,Bool)]

--Questao 4
calcular :: Memoria -> ExpBool -> Bool
calcular [] _           = False
calcular _ (Cte b)      = b
calcular expr (Var b)   = case lookup b expr of
                                Nothing -> False
                                Just x -> x
calcular expr (Neg b)   = not (calcular expr b)
calcular expr (Con b c) = calcular expr b && calcular expr c
calcular expr (Dis b c) = calcular expr b || calcular expr c

--Questao 5
lista :: ExpBool -> [String]
lista (Cte _)   = []
lista (Var b)   = b : []
lista (Neg b)   = lista b
lista (Con b c) = nub $ sort (lista b ++ lista c)
lista (Dis b c) = lista (Con b c)

--Questao 7
converter :: (Memoria,Bool) -> String
converter ((_,bool1):[],bool2) = show bool1 ++ "\t:" ++ show bool2
converter ((_,bool1):xs,bool2) = show bool1 ++ "\t" ++ converter (xs,bool2)

converterToString :: TabelaVerdade -> String
converterToString ((mem,bool):[]) = converter(mem,bool) ++ "\n"
converterToString ((mem,bool):xs) = converter(mem,bool) ++ "\n" ++ converterToString xs

--Questao 8
listaTabela :: ExpBool -> [Memoria] -> [Bool]
listaTabela _ []     = []
listaTabela expr (x:xs) = (calcular x expr) : listaTabela expr xs

listaMem :: ExpBool -> [[Bool]] -> [Memoria]
listaMem expr []     = []
listaMem expr (x:xs) = (zip (lista expr) x) : listaMem expr xs

converterToTabela :: ExpBool -> TabelaVerdade
converterToTabela expr = zip (mem)(listaTabela expr mem)
                            where mem = listaMem expr (replicateM (length $ lista expr)[False,True])

dadosFormatados :: [String] -> String
dadosFormatados []     = "";
dadosFormatados (x:xs) = x ++ "  " ++ dadosFormatados xs

converterToString2 :: ExpBool -> String
converterToString2 expr = converterToString $ converterToTabela expr
