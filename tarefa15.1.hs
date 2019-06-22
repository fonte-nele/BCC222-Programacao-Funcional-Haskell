module Main where

import System.IO(stdout, hSetBuffering,BufferMode(NoBuffering))
import Data.Char(toLower)
import System.Random(randomRIO)

simOuNao :: String->IO Bool
simOuNao pergunta =
    do putStr pergunta
       resposta <- getLine
       case map toLower resposta of
        "s" -> return True
        "n" -> return False
        _   -> do putStrLn "(IDIOTA)"
                  simOuNao pergunta

jogar :: IO()
jogar =
    do n <- randomRIO (1,1000)
       acertar n
       resposta <- simOuNao "Deseja jogar novamente?"
       if resposta
           then jogar
           else return ()

acertar :: Int -> IO()
acertar secreto =
    do putStr "Digite um numero entre 1 e 1000: "
       palpite <- readLn
       if palpite == secreto
           then putStrLn "Parabens, você acertou"
           else do putStrLn (if palpite < secreto
                            then "Muito pequeno"
                            else "Muito grande")
                   putStrLn "TENTE NOVAMENTE\n"
                   acertar secreto

main =
    do hSetBuffering stdout NoBuffering
       putStrLn "Adivinha o número v1.0"
       putStrLn"========================================="
       jogar

      -- ghc -- make tarefa15.1
