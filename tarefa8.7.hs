fat :: Int->Int
fat x | x<=1 = 1
      | x==2 = 2
      | otherwise = x* fat(x-2)
