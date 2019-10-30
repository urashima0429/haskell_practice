-- x:radian
sine x =
    let 
        n = 10
        sinei sum ith i = 
            if i > n 
                then sum 
                else sinei (sum+ith) (ith*(-1)*x*x/(2*i)/(2*i+1)) (i+1) 
    in sinei 0 x 1

-- x:radian
cosine x =
    let 
        n = 10
        cosinei sum ith i = 
            if i > n 
                then sum 
                else cosinei (sum+ith) (ith*(-1)*x*x/(2*i-1)/(2*i)) (i+1) 
    in cosinei 0 1 1
