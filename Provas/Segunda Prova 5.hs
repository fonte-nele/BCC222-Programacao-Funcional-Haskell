module Main (main) where

import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))

main :: IO ()
main =
  do hSetBuffering stdout NoBuffering
     putStrLn "Cálculo da média geométrica"
     putStrLn "==================================="
     putStrLn "Digite uma sequência de números positivos"
     putStrLn "Termine com um número não positivo"
     numeros <- leLista
     print numeros
     let n = length numeros
     print n
     if n > 0
       then do let mediaG = (product numeros) ** (1 / fromIntegral n)
               putStrLn ("Média geométrica: " ++ show mediaG)
       else putStrLn "Sequência vazia"

leLista :: IO [Double]
leLista =
  do x <- readLn
     if x > 0
       then do xs <- leLista
               return (x:xs)
       else return []
