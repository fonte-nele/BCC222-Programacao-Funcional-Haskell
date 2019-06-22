soma :: Int->Int->Int
soma m n | n==0 = m
         | otherwise = soma (succ m) (pred n)
