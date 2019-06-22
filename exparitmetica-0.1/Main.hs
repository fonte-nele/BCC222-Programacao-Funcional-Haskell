module Main where

import System.IO (stdout,hSetBuffering,BufferMode(NoBuffering))
import Text.Parsec (parse)

import Exp (Exp)
import Parser (parseExp)
import Eval (eval, runReader)

main = do hSetBuffering stdout NoBuffering
          calc 1

calc n = do putStr ("[" ++ show n ++ "] ")
            input <- getLine
            case parse parseExp "-" input of
              Left err -> putStrLn (show err)
              Right exp -> do putStrLn (show exp)
                              putStrLn (show (runReader (eval exp) []))
            calc (n+1)
