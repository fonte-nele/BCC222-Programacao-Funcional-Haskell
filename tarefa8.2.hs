potdois :: Integer->Integer
potdois n | n==0 = 1
          | otherwise = 2* potdois(n-1)
