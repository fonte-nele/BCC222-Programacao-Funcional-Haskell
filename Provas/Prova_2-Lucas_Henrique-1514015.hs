-- Questao 1
selEnquanto :: Eq a => (a->Bool) -> [a] -> [a]
selEnquanto f (x:xs) =  selEnquanto' (map f (x:xs))

selEnquanto' :: [Bool] -> [Bool]
selEnquanto' (x:xs) = case x of
                        True -> selEnquanto' xs
                        False -> xs

-- Questao 2
type Nome = String
type Telefone = String
type Agenda = [(Nome, Telefone)]

minhaAgenda :: Agenda
minhaAgenda =
  [ ("Ana Maria","3213213"),
    ("Paulo","987987"),
    ("Rodrigo","54654"),
    ("Felipe","987697"),
    ("Felipe","65457"),
    ("Marina","4654"),
    ("Felipe","798765")
  ]

  achaContatos :: Nome -> Agenda -> [Telefone]
  achaContatos nome x:xs = filter (\f = if (nome == fst x)
                                          then return snd x
                                          else return) xs

  -- Questao 3



  --Questao 4
  {-
    getLine retorna o resultado de uma ação de E/S
    e não uma String, que o esperado por toUpper, com isso precisamos converter
    o resultado dessa ação para string antes de poder usa-lo em toUpper.
  -}


--Questao 5
module Main (main) where

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Text.Printf (printf)

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          putStr "Digite os numeros "
          n <- readLn
          lista <- leNumeros n
          let lista' = (reverse (drop 1 (reverse lista)))
          let medGeo = calculaMed lista'
          putStrLn (printf "Media geometrica: %f" medGeo)
          return ()

leNumeros :: Float -> IO [Float]
leNumeros n
  | n <= 0     = return []
  | otherwise = do n <- readLn
                   outrosAlunos <- leNumeros n
                   return ( n : outrosAlunos )

calculaMed :: [Float] -> Float
calculaMed x = ((product x)**(1.0 / fromIntegral (length x)))
