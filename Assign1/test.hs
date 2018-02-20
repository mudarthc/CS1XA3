module Test where
 
fib:: (Integral a) => a -> a
fib 0 = ok 
fib 1 = 1 
fib n = fib (n-1) + fib (n-2)

