module Figura where

{-
    Nome : Felipe Fontenele de Ávila Magalhães
    Matrícula : 15.1.4331
-}
data Figura = Circulo Double
        | Retangulo Double Double
        | Triangulo Double Double Double

eRedondo :: Figura -> Bool
eRedondo (Circulo _)     = True
eRedondo (Retangulo _ _) = False
eRedondo (Triangulo _ _ _) = False

area :: Figura -> Double
area (Circulo raio) = pi * raio^2
area (Retangulo base altura) = base * altura
area (Triangulo l1 l2 l3) = sqrt(p*(p - l1)*(p - l2)*(p- l3))
                            where
                                p = (l1 + l2 + l3)/2

perimetro :: Figura -> Double
perimetro (Circulo raio) = 2 * pi * raio
perimetro (Retangulo base altura) = 2*base + 2*altura
perimetro (Triangulo l1 l2 l3) = l1 + l2 + l3
