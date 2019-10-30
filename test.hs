
f(x) = 2 * x + 1
g(x,y) = x^2 - 3*x*y + 2*y^2

fact :: Integer -> Integer
fact n = 
    if n == 0 then 1 
    else n * fact (n-1)

fact2 0 = 1
fact2 n = n * fact2 (n - 1)

expt n m = 
    if m == 0 then 1
    else if mod m 2 == 0 then expt (n * n) (div m 2)
    else n * expt (n * n) (div m 2)
expt2 n m | m == 0          = 1
          | m == 1          = n
          | mod m 2 == 0    = expt2 (n*n) (div m 2)
          | otherwise       = n * expt2 (n*n) (div m 2)

e_ i n x = 
    if i > n then 0 
    else (1/x) + e_ (i+1) n ((i+1)*x)

e n = e_ 0 n 1

comb n m =
    if m == 0 || n == m then 1
    else comb (n-1) m + comb (n-1) (m-1)

subsquare n = 
    if n <= 0 then 0
    else n*n + subsquare (n-1)

fib n = 
    if n <= 2 then 1
    else fib (n-1) + fib (n-2)

euclid n m =
    if m == 0 then n
    else euclid m (mod n m)

-- x:radian n:num of terms
sine x n =
    let 
        sinei sum ith i = 
            if i > n 
                then sum 
                else sinei (sum+ith) (ith*(-1)*x*x/(2*i)/(2*i+1)) (i+1) 
    in sinei 0 x 1

-- x:radian n:num of terms
cosine x n =
    let 
        cosinei sum ith i = 
            if i > n 
                then sum 
                else cosinei (sum+ith) (ith*(-1)*x*x/(2*i-1)/(2*i)) (i+1) 
    in cosinei 0 1 1


nthPrime n = 
    let
        screening (p:xs) = p : screening [x | x <- xs, x `mod` p /= 0]
        primes = screening [2..]
    in primes !! (n-1)