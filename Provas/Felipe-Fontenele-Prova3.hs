{-
  Nome: Felipe Fontenele de Ávila Magalhães
  Matrícula: 15.1.4331
-}

import Data.List

--Questao 1!!!
aplica :: (a->b)->[a]->[(a,b)]
aplica fun lista = [(x, fun x)| x <- lista]


--Questao 2!!!
--Letra a
data Expr = Cte Bool            --constantes
         | Var String           --variaveis
         | Neg Expr             --negacao
         | Con Expr Expr        --conjuncao (e)
         | Dis Expr Expr        --disjuncao (ou)
         deriving (Show)
--Letra b
p = Var "p"
q = Var "q"
a = Var "a"
b = Var "b"
c = Var "c"
expr1,expr2 :: Expr
expr1 = (Dis p (Con (Neg q) p))
expr2 = (Con (Neg (Dis a b)) (Dis (Neg b) (Neg (Con a c))))

--Letra c
nvars :: Expr -> Int
nvars exp = length(nvars1 exp)

--Monta a lista para depois saber o tamanho da mesma
nvars1 :: Expr -> [String]
nvras1 (Cte _)   = []
nvars1 (Var b)   = b : []
nvars1 (Neg b)   = nvars1 b
nvars1 (Con b c) = sort (nvars1 b ++ nvars1 c)
nvars1 (Dis b c) = nvars1 (Con b c)


--Questao 3!!!
--Letra a
data Arv t = V
          | N t [Arv t]
          deriving (Show)
--exemplo :: Arv
--exemplo = (V "ivo")

--Letra b
profundidade :: Arv t -> Int
profundidade V = 0
--profundidade (N _ (x:xs)) = 1 + max(xs)

--Letra c
tamanho :: Arv t -> Int
tamanho V = 0
tamanho (N _ (x:xs)) = 1 + length(xs)

--Letra d
instance Eq t => Eq (Arv t) where
  V == V                          = True
  (N x1 [l1]) == (N x2 [l2])      = x1==x2 && l1==l2
  _ == _                          = False

--FIM RESOLUCAO PROVA!
