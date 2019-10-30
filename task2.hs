nthPrime n = 
    let
        screening (p:xs) = p : screening [x | x <- xs, x `mod` p /= 0]
        primes = screening [2..]
    in primes !! (n-1)