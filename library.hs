fact :: Integer -> Integer
fact 0 = 1 
fact n = n * fact (n-1)

comb :: Integer -> Integer -> Integer
comb n m =
    if m == 0 || n == m then 1
    else comb (n-1) m + comb (n-1) (m-1)

