quociente :: Int->Int->Int
quociente a b | a<b = 0
              | otherwise = 1 + quociente(a-b)(b)

resto :: Int->Int->Int
resto a b | a<b = a
          | otherwise = resto(a-b)(b)
