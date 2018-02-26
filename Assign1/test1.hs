module Test1 where

fib:: (Integral a) => a -> a
fib 0 = nope
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
