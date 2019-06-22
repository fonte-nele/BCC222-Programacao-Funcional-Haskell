potdois :: Integer->Integer
potdois n = pdois n 1

pdois :: Integer->Integer->Integer
pdois n y | n==0 = y
          | n>0  = pdois(n-1)(2*y)
