--importando funcao nub que remove duplicacoes (usada na questao 2)
import Data.List(nub) 

--Questao 1
aplica :: (a -> b) -> [a] -> [(a,b)]
aplica f l = [(x,y) | x <- l, y <- [f x]]

--Questao 2
data Expr = Cte Bool | Var String | Neg Expr | Con Expr Expr | Dis Expr Expr
    deriving (Show)

expr1 :: Expr
expr1 = Dis (Var "p") (Con (Neg (Var "q")) (Var "p"))

expr2 :: Expr
expr2 = Con (Dis (Var "a") (Var "b")) (Dis (Neg (Var "b")) (Neg (Con (Var "a") (Var "c"))))

nvars :: Expr -> Int
nvars expr = length(nub (vars expr))
    where vars :: Expr -> [String]
          vars (Cte x)   = []
          vars (Var x)   = [x]
          vars (Neg x)   = vars x
          vars (Con x y) = (vars x) ++ (vars y)
          vars (Dis x y) = (vars x) ++ (vars y)

--Questao 3
data Arv t = V | N t [Arv t]

exemplo :: Arv String
exemplo = N "ivo" 
            [N "ana" 
                [N "maria" [V], N "pedro" [V]], 
            N "miguel" 
                [N "tales" 
                    [N "carla" [V], N "vanda" [V], N "igor" [V]]], 
            N "joana" 
                [N "fabio" [V]]]

profundidade :: Arv t -> Int
profundidade (V)     = 0
profundidade (N x y) = 1 + maximoLista (map profundidade y)
    where maximoLista = foldl1 max

tamanho :: Arv t -> Int
tamanho (V)     = 0
tamanho (N x y) = 1 + sum (map tamanho y)

instance (Eq t) => Eq (Arv t) where
    (V) == (V) = True
    (N x y) == (N z w) = x == z && y == w
    _ == _  = False