module Main (main) where

import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))

versao :: String
versao = "0.1"

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          putStrLn "Sistema de caixa automático"
          putStrLn ("v" ++ versao)
          menu

menu :: IO ()
menu = do putStrLn "=============================="
          putStrLn "Banco BCC222"
          putStrLn "=============================="
          putStrLn "Opções:"
          putStrLn "1. Saldo"
          putStrLn "2. Extrato"
          putStrLn "3. Depósito"
          putStrLn "4. Saque"
          putStrLn "5. Fim"
          putStrLn ""
          putStr "Escolha uma opção: "
          opcao <- readLn
          putStrLn ""
          case opcao of
            1 -> putStrLn "Não implementado"
            2 -> putStrLn "Não implementado"
            3 -> putStrLn "Não implementado"
            4 -> putStrLn "Não implementado"
            5 -> putStrLn "Obrigado por usar o nosso banco"
            _ -> putStrLn "Opção inválida"
          putStrLn ""
          if opcao /= 5
            then menu
            else return ()

                    
