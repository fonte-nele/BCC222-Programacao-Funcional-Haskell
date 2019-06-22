fatorial :: Integer->Integer
fatorial n | n == 0 = 1
           | n>0 = fatorial(n-1)*n
