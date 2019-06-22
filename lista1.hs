zipp :: [a] -> [b] -> [(a,b)]
zipp [] _ = []
zipp _ [] = []
zipp (x:xs) (y:ys) = (x,y) : zipp xs ys

raizes :: Floating a => a->a->a->(a,a)
raizes a b c = (x1,x2)
    where
        delta = b^2 - 4*a*c
        x1 = (-b + sqrt delta) / (2*a)
        x2 = (-b - sqrt delta) / (2*a)

maximo :: Ord a=> [a]->a
maximo (x:[]) = x
maximo (x:xs) = if x > maximo xs then x
                    else maximo xs

buscabin :: Ord a=> [a]->a->Bool
buscabin [] _ = False
buscabin [x] e | e == meio = True
               | e<meio = buscabin esquerda e
               | otherwise = buscabin direita e
               where posMeio = div(length xs) 2
                    meio = indice xs posMeio
                    (esquerda,direira) = splitAt posMeio xs
