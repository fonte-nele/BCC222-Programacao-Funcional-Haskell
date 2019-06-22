module Main(main) where

import System.Random (randomRIO)

main:: IO()
main =
    do putStrLn "Lancamento de dois dados"
        x <-
