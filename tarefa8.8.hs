mult :: Integer->Integer->Integer
mult m n | m>n = 1
         | otherwise = m * mult(m+1) n
