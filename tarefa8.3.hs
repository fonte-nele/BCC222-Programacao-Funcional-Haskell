mult :: Int->Int->Int
mult a b | b == 0 = 0
         | b>0    = a + mult a (b-1)
         | otherwise = negate (mult a (negate b))
