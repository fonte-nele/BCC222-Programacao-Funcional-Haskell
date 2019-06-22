module Main(main) where
{-
    Nome: Felipe Fontenele de Ávila Magalhães
    Matrícula: 15.1.4331
    Obs: Essa Tabela Verdade pode ser gerada atraves de uma simples funcao
        chamada Hatt com o import Data.Logic.Propositional com a geração das tabelas e tal!
-}

import System.IO (stdout,hSetBuffering,BufferMode(NoBuffering))

import ExpBool
import ParseExpBool

main =
     do hSetBuffering stdout NoBuffering
        putStr "\n"
        putStrLn "                      Avaliando expressoes booleanas"
        putStrLn "=========================================================================="
        putStr "\nDigite uma expressao booleana: "
        expr <- getLine
        case parseExpBool expr of
                Nothing    -> do putStrLn "Expressao digitada invalida!"
                Just dados -> do putStrLn (dadosFormatados $ lista dados)
                                 putStr (converterToString2 dados)
