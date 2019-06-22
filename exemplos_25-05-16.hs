zipp :: [a] -> [b] -> [(a,b)]
zipp [] _ = []
zipp _ [] = []
zipp (x:xs) (y:ys) = (x,y):zipp xs ys

--zipp [1,2,3] ['a','b']  = [(1,'a'),(2,'b')]


raizes :: Floating a => a -> a -> a -> (a,a)
raizes a b c = let delta = b^2 - 4*a*c
                   x1 = (-b + sqrt delta)/(2*a)
                   x2 = (-b - sqrt delta)/(2*a)
                   in (x1,x2)

raizes' :: Floating a => a -> a -> a -> (a,a)
raizes' a b c = (x1,x2)
  where
    delta = b^2 - 4*a*c
    x1 = (-b + sqrt delta)/(2*a)
    x2 = (-b - sqrt delta)/(2*a)


maximo :: Ord a => [a] -> a
maximo [x] = x
maximo (x:xs) = if x > maximo xs then x
   else maximo xs
minimo:: Ord a => [a] -> a
minimo (x:[]) = x
minimo (x:xs) = if x < maximo xs then x
  else maximo xs


indice :: [a] -> Int -> a
indice (x:_) 0 = x
indice (x:xs) n = indice xs (n-1)

procura :: Eq a => [a] -> a -> Integer
procura (x:xs) e | e == x = 0
                 | otherwise = 1 + procura xs e

remove :: Eq a => [a] -> a -> [a]
remove (x:xs) e | x == e = xs
                | otherwise = x:remove xs e

quebra :: Eq a => [a] -> a -> ([a],[a])
quebra (x:xs) e | x == e = ([], xs)
                | otherwise  = (x:fst resto, snd resto)
                    where resto = quebra xs e

quebra' :: Eq a => [a] -> a -> ([a],[a])
quebra' (x:xs) e | x == e = ([], x:xs)
                 | otherwise  = (x:fst resto, snd resto)
                    where resto = quebra' xs e

buscabin :: Ord a => [a] -> a -> Bool
buscabin [] _ = False
buscabin [x] e = e == x -- if e == x then True else False
buscabin xs e | e == meio = True
              | e < meio = buscabin esquerda  e
              | otherwise = buscabin direita e
              where posMeio = div (length xs) 2
                    meio = indice xs posMeio
                    (esquerda, direita) = splitAt posMeio xs
