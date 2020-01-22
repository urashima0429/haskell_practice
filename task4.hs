polyplus as bs = 
    let 
        paddingZip _ [] [] = []
        paddingZip n [] (y:ys) = (n, y):(paddingZip n [] ys)
        paddingZip n (x:xs) [] = (x, n):(paddingZip n xs [])
        paddingZip n (x:xs) (y:ys) = (x, y):(paddingZip n xs ys)
    in
    map (uncurry (+)) (paddingZip 0 as bs)

polytimes as bs =
    let
        fPadding n xs = if n <= 0 then xs else fPadding (n-1) (0:xs)
        polytimesi [] ys = []
        polytimesi (x:xs) ys = (sum (map (uncurry (*)) (zip (x:xs) ys))):(polytimesi xs ys)
    in polytimesi (fPadding (length bs - 1) as) (reverse bs)
    