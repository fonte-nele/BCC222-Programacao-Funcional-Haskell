fib :: Integer->Integer
fib n = fib' n 0 1
    where
        fib' n a b
            | n==0 = a
            | n==1 = b
            | n>=2 = fib' (n-1) b (a+b)
