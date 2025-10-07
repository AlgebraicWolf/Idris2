module Main

fib : Nat -> Nat
fib 0 = 0
fib 1 = 1
fib (S (S n)) = fib (S n) + fib n

main : IO ()
main = print (fib 30 + fib 31)
