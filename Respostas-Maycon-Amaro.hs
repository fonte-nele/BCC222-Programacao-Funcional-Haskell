module Main (main) where

import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))

--Questao 01
selEnquanto :: (a -> Bool) -> [a] -> [a]
selEnquanto f []     = []
selEnquanto f (x:xs) = if (f x)
                          then x : selEnquanto f xs
                          else []
                          
--Questao 02                    
type Nome       = String
type Telefone   = String
type Agenda     = [(Nome, Telefone)]

minhaAgenda :: Agenda
minhaAgenda = [("Ana Maria", "3551-6228"),
               ("Paulo", "3551-0222"),
               ("Rodrigo", "3552-2322"),
               ("Felipe", "3551-5677"),
               ("Felipe", "999-8888"),
               ("Marina", "3551-9999"),
               ("Felipe", "9876-5432")]

colheTelefone :: Nome -> (Nome, Telefone) -> Telefone
colheTelefone nome (name, tel) = if (nome == name)
                                    then tel
                                    else ""

achaContatos :: Nome -> Agenda -> [Telefone]
achaContatos nome agnda = filter (not . null) (map (colheTelefone nome) agnda)

--Questao 03
type Time       = String
type Gols       = Int
type Jogo       = ((Time, Gols), (Time, Gols))
type Campeonato = [Jogo]
type Pontos     = Int
type Tabela     = [(Time, Pontos)]

campMineiro :: Campeonato
campMineiro = [(("cruzeiro", 0), ("atletico", 0)),
               (("uberlandia", 5), ("america", 1)),
               (("atletico", 1), ("america", 2)),
               (("uberlandia", 2), ("cruzeiro", 1)),
               (("america", 4), ("cruzeiro", 3)),
               (("urt", 0), ("atletico", 1)),
               (("urt", 2), ("america", 2))]

existeNaLista :: Tabela -> (Time, Pontos) -> Bool
existeNaLista [] _                          = False                                                                                                                                     
existeNaLista ((t, p):xs) (time, pontos)    = (t == time) || existeNaLista xs (time, pontos)

somaPontos :: Tabela -> (Time, Pontos) -> Tabela
somaPontos [] _                       = []
somaPontos ((t, p):xs) (time, pontos) = if (t == time)
                                           then (t, p + pontos) : somaPontos xs (time, pontos)
                                           else (t, p) : somaPontos xs (time, pontos)

poeNaTabela :: Tabela -> (Time, Pontos) -> Tabela
poeNaTabela lista par = if existeNaLista lista par
                           then somaPontos lista par
                           else reverse (par : reverse lista)

computaJogo :: Tabela -> Jogo -> Tabela
computaJogo lista ((t1, g1), (t2, g2)) = if g1 > g2
                                            then poeNaTabela lista (t1, 3)
                                            else if g1 < g2
                                                    then poeNaTabela lista (t2, 3)
                                                    else poeNaTabela (poeNaTabela lista (t1, 1)) (t2, 1)
                                                    
pontos :: Campeonato -> Tabela
pontos camp = foldl computaJogo [] camp

--Questao 04
--A função length não pode ser usada desta forma no escopo do 'DO', porque não é uma função do tipo 'IO a', sendo 'a' um tipo qualquer. É necessário que haja uma variável recebendo o valor de length através do operador '<-'

--Questao 05
main = do hSetBuffering stdout NoBuffering
          putStrLn "Cálculo da média geométrica"
          putStrLn "---------------------------"
          putStrLn "Digite uma sequência de números"
          putStrLn "Para terminar, digite um valor negativo"
          lista <- lerLista
          case lista of
             [] -> putStrLn "Sequência vazia"
             _  -> do putStrLn "A média dos números digitados é "
                      print ((product lista) ** (potenciaFracionaria lista))

potenciaFracionaria :: (Num a) => [a] -> Float
potenciaFracionaria lista = 1 / fromIntegral (length lista)

lerLista = do x <- readLn
              if x <= 0
                 then return []
                 else do xs <- lerLista
                         return (x:xs)
                         

