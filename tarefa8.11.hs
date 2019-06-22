pot :: Integer->Integer->Integer
pot x n | n==0 = 1
        | otherwise = x * pot x (n-1)
