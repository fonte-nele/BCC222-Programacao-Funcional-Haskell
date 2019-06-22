module Main where

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

main :: IO ()
main =
    do hSetBuffering stdout NoBuffering
       putStr "Digite a nota do trabalho de laboratorio ...: "
       laboratorio <- readLn
       putStr "Digite a nota da avaliacao semestral .......: "
       semestral <- readLn
       putStr "Digite a nota do exame final ...............: "
       final <- readLn
       putStrLn ""
       putStr "Conceito obtido: "
       putStrLn [conceito laboratorio semestral final]

conceito :: Float -> Float -> Float -> Char
conceito a b c | valor >= 8.0 = 'A'
               | valor >= 7.0 = 'B'
               | valor >= 6.0 = 'C'
               | valor >= 5.0 = 'D'
               | otherwise = 'E'
               where
                   valor = (2 * a + 3 * b + 5 * c) / (2+3+5)
