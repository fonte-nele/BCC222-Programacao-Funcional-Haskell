module Main(main) where

import System.IO (stdout, hSetBuffering,BufferMode(NoBuffering))
import Text.Read (readMaybe)

main :: IO ()
main =
    do hSetBuffering stdout NoBuffering
       putStrLn "Conversao de temperaturas"
       putStrLn "========================"
       c <- letTemperatura
       let f = 32 + 9/5 * c
       putStrLn ("Temperatura em ºF: " ++ show f)

letTemperatura :: IO Double
letTemperatura =
    do putStr "Digite a temperatura em ºC: "
       entrada <- getLine
       case readMaybe entrada of
            Just x -> return x
            Nothing -> do putStrLn "Tente Novamente!!"
                          letTemperatura
