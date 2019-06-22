raiz :: Integer->Integer
raiz n = valor n n
        where
            valor n i
                | i^2 > n = valor n (i-1)
                | otherwise = i
