nthPrime :: Integer -> Integer
-- nthPrime n = 
--     let 
--         screening i (p:xs) = if i <= 1 then p else screening (i-1) [x | x <- xs, x `mod` p /= 0]
--     in screening n [2..]

nthPrime n =
    let 
        nthPrimei i x =
            let 
                isPrime n =
                    let
                        isPrimei i = 
                            if i*i>n then True 
                            else 
                                if n `mod` i == 0 then False 
                                else isPrimei (i+1)
                    in 
                    n > 1 && isPrimei 2
            in
            if i > n then (x-1)
            else
                if isPrime x then nthPrimei (i+1) (x+1)
                else nthPrimei i (x+1)
    in nthPrimei 1 2

