{-
  Nome: Felipe Fontenele de Ávila Magalhães
  Matrícula: 15.1.4331
-}

--Prova Parcial 1!!!

--Questão 1!
quadrante :: (Num a, Ord a) => a -> Int
quadrante x | x < 0               = quadrante (x + 360)
            | x > 0 && x < 90     = 1
            | x > 90 && x < 180   = 2
            | x > 180 && x < 270  = 3
            | x > 270 && x < 360  = 4
            | x > 360             = quadrante (x - 360)
            | otherwise           = 0

----------------------------------------------------------------------------------------------------------

--Questão 2!
carregamento :: (Floating a, Ord a) => a -> a -> a -> a -> [(a, a)]
carregamento v r c carga = cargaaux v r c carga 0.0

cargaaux :: (Floating a, Ord a) => a -> a -> a -> a -> a -> [(a, a)]
cargaaux v r c carga tempo | carga2 >= carga = [(tempo,carga2)]
                           | otherwise       = [(tempo,carga2)] ++ cargaaux v r c carga (tempo + 0.1)
                              where
                                carga2 = (c * v)*(1 - exp(-tempo/(r*c)))
