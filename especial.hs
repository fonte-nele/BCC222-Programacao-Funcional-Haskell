--Questão 1!
quadrante :: (Floating a, Ord a) => a -> String
quadrante x | x <= 0              = "Fora do intervalo"
            | x < 90              = "Primeiro quadrante"
            | x > 90 && x < 180   = "Segundo quadrante"
            | x > 180 && x < 270  = "Terceiro quadrante"
            | x > 270 && x < 360  = "Quarto quadrante"
            | x >= 360            = "Fora do intervalo"
            | otherwise           = "Zero"

--Questão 2!
melhorou :: (Num a, Ord b) => [b] -> [b] -> a
melhorou [] []         = 0
melhorou (x:xs) (y:ys) | y > x     = 1 + melhorou (xs) (ys)
                       | otherwise = melhorou (xs) (ys)

--Questao 3!
--carregamento :: (Fractional b, Fractional t, Fractional t1, Integral b, Ord t1) => t1 -> b -> t1 -> t1 -> [(t, t1)]
carregamento v r c carga | carga2 >= carga = []
                         | otherwise       = [(tempo,carga2)] ++ carregamento v r c carga
                            where
                                tempo = 0.00
                                carga2 = (c * v)*(1 - (2.718182)**(-tempo/(r*c)))

--Questao 4!
grau_pol :: (Fractional a, Ord a) => [a] -> Int
grau_pol []     = 0
grau_pol (x:xs) | x /= 0.0   = (length(x:xs) - 1)
                | otherwise  = grau_pol xs

soma_pol :: Fractional a => [a] -> [a] -> [a]
soma_pol [] []         = []
soma_pol (x:xs) (y:ys) | length(x:xs) > length(y:ys) = [x] ++ soma_pol xs (y:ys)
                       | length(x:xs) < length(y:ys) = [y] ++ soma_pol (x:xs) ys
                       | otherwise                   = [x+y] ++ soma_pol xs ys
