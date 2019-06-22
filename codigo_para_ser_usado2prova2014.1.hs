module Main (main) where

import System.IO ( stdout, hSetBuffering, BufferMode(NoBuffering) )
import System.Random ( randomRIO )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

main :: IO ()
main =
  do hSetBuffering stdout NoBuffering
     putStrLn "Adivinhe o número v1.0"
     putStrLn "=========================================="
     args <- getArgs
     case args of
       [arg1,arg2] -> do let p = read arg1
                         let q = read arg2
                         numero <- randomRIO (p,q)
                         acertar p q numero
       _ -> do prog <- getProgName
               putStrLn "Chamada incorreta do programa"
               putStrLn ("Uso: " ++ prog ++ " <limite inferior> <limite superior>")
               exitFailure

acertar :: Int -> Int -> Int -> IO ()
acertar p q numero =
  do putStrLn ""
     putStr ("Adivinhe um número entre " ++ show p ++ " e " ++ show q ++ ": ")
     x <- readLn
     if x == numero
       then putStr "Parabéns, você adivinhou o número"
       else do putStr (if x < numero then "Muito baixo." else "Muito alto.")
               putStrLn " Tente novamente"
               acertar p q numero