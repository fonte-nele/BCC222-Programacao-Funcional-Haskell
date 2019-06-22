module Main (main) where

import System.IO(hSetBuffering, stdout, BufferMode(NoBuffering))
import Text.Printf(printf)

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          putStrLn "Fechamento de notas de uma disciplina"
          putStrLn "====================================="
          putStr "Quantidade de alunos: "
          n <- readLn :: IO Int
          lista <- dadosAlunos n 1
          listaMedias <- situacaoAlunos 1 lista
          let somaMedias = sum listaMedias
          let mediaGeral = somaMedias / fromIntegral n
          putStrLn (printf "Media Geral: %.2f"mediaGeral)
          return ()

dadosAlunos :: Int->Int->IO[(Float,Float,Float)]
dadosAlunos n i
            | i>n = return []
            | otherwise = do putStrLn (printf "Aluno %d"i)
                             putStr "   Nota 1:"
                             n1 <- readLn
                             putStr "   Nota 2:"
                             n2 <- readLn
                             putStr "   Nota 3:"
                             n3 <- readLn
                             outrosAlunos <- dadosAlunos n (i+1)
                             return ((n1,n2,n3) :outrosAlunos)

situacaoAlunos :: Int->[(Float,Float,Float)]->IO[Float]
situacaoAlunos _ [] = return []
situacaoAlunos i ((a,b,c):resto) = do let m = (a+b+c)/3
                                          sit | m<3 = "reprovado"
                                              | m<7 = "exame especial"
                                              | otherwise = "aprovado"
                                      putStrLn (printf"Média do aluno %d: %.2f %s"i m sit)
                                      ms <- situacaoAlunos (i+1) resto
                                      return (m:ms)

filter :: (a->Bool)->[a] -> [a]
map :: (a->b)->[a] -> [b]

compose f g = h
    where
        h x = f(g x)

compose f g x = f (g x)
compose f g = \x->f(gx)
