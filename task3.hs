delete d [] = []
delete d (x:xs) = if x == d then xs else x:delete d xs

deleteAll d [] = []
deleteAll d (x:xs) = if x == d then deleteAll d xs else x:(deleteAll d xs)


deviation [] = 0
deviation xs = 
    let len = fromIntegral (length xs) in
    let    
        ave = (sum xs) / len
        sumSq [] = 0
        sumSq (x:xs) = x*x + sumSq xs
    in 
    sqrt ((sumSq xs / len) - ave*ave)

lastSegment [] _ = True
lastSegment xs [] = False
lastSegment xs (y:ys) = if xs == ys then True else lastSegment xs ys

larger x [] = []
larger x (y:ys) = if y >= x then y:(larger x ys) else larger x ys
smaller x [] = []
smaller x (y:ys) = if y < x then y:(smaller x ys) else smaller x ys

-- quick sort like
seiretsu [] = []
seiretsu (x:xs) =
    let
        larger x [] = []
        larger x (y:ys) = if y >= x then y:(larger x ys) else larger x ys
        smaller x [] = []
        smaller x (y:ys) = if y < x then y:(smaller x ys) else smaller x ys
    in seiretsu (smaller x xs) ++ [x] ++ seiretsu (larger x xs)

narabi xs = 
    let
        -- get (nth num, the rest of list)
        cut n l (c:r) = 
            if n <= 0 then (c, (reverse l)++r)
            else cut (n-1) (c:l) r
    in
    let 
        perm n seq [] = [seq]
        perm n seq rest =
            if n >= length rest then []
            else  
                let (a, b) = cut n [] rest in
                perm 0 (seq++[a]) b ++ perm (n+1) seq rest
    in perm 0 [] xs


sentaku xs n =
    let 
        comb seq [] = if length seq == n then [seq] else []
        comb seq (e:rest) =
            if length seq + length rest < n then [seq++e:rest]
            else comb (seq++[e]) rest ++ comb seq rest 
    in comb [] xs