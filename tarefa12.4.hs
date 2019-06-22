module Main where

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

celsius :: Double -> Double
celsius f = 5/9 * (f-32)

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          putStr "Temperatura em Fahrenheit: "
          f <- getLine
          putStrLn ("Temperatura em Celsius: " ++ (show (celsius(read f))))
