
f(x) = 2 * x + 1
g(x,y) = x^2 - 3*x*y + 2*y^2

-- fact :: Integer -> Integer
-- fact n = 
--     if n == 0 then 1 
--     else n * fact (n-1)

fact2 0 = 1
fact2 n = n * fact2 (n - 1)

-- expt n m = 
--     if m == 0 then 1
--     else if mod m 2 == 0 then expt (n * n) (div m 2)
--     else n * expt (n * n) (div m 2)
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

-- fib n = 
--     if n <= 2 then 1
--     else fib (n-1) + fib (n-2)

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


_nthPrime n = 
    let
        screening (p:xs) = p : screening [x | x <- xs, x `mod` p /= 0]
        primes = screening [2..]
    in primes !! (n-1)


nthPrime n = 
    let 
        screening i (p:xs) = if i <= 1 then p else screening (i-1) [x | x <- xs, x `mod` p /= 0]
    in screening n [2..]   


-- nthPrime :: Integer -> Integer
-- nthPrime n =
--     let 
--         nthPrimei i x =
--             let 
--                 isPrime n =
--                     let
--                         isPrimei i = 
--                             if i*i>n then True 
--                             else 
--                                 if n `mod` i == 0 then False 
--                                 else isPrimei (i+1)
--                     in 
--                     n > 1 && isPrimei 2
--             in
--             if i > n then (x-1)
--             else
--                 if isPrime x then nthPrimei (i+1) (x+1)
--                 else nthPrimei i (x+1)
--     in nthPrimei 1 2

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
seiretu [] = []
seiretu (x:xs) =
    let
        larger x [] = []
        larger x (y:ys) = if y >= x then y:(larger x ys) else larger x ys
        smaller x [] = []
        smaller x (y:ys) = if y < x then y:(smaller x ys) else smaller x ys
    in seiretu (smaller x xs) ++ [x] ++ seiretu (larger x xs)

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
  




convert num =
    let pow x n = if n == 0 then 1 else x * pow x (n-1) in
    let countDigits n = if n == 0 then 0 else 1 + countDigits (n `div` 10) in
    let 
        mapNum 0 = ""
        mapNum 1 = "壱"
        mapNum 2 = "弐"
        mapNum 3 = "参"
        mapNum 4 = "四"
        mapNum 5 = "五"
        mapNum 6 = "六"
        mapNum 7 = "七"
        mapNum 8 = "八"
        mapNum 9 = "九"
    in
    let 
        mapDigit d = 
                 if d `mod` 4 == 1 then "拾"
            else if d `mod` 4 == 2 then "百"
            else if d `mod` 4 == 3 then "千"
            else if d == 4 then "万"
            else if d == 8 then "億"
            else ""
    in 
    let 
        mapDigitNum d n = 
            if d == 0 then mapNum n
            else (if n == 1 then "" else mapNum n) 
              ++ (if n == 0 then "" else mapDigit d)
    in
    let
        convert4 d n = 
            if d < 0 then ""
            else mapDigitNum d (n `div` (pow 10 d)) ++ convert4 (d-1) (n `mod` pow 10 d)
    in 
    let 
        cutting n i = 
            if i < 0 then ""
            else 
                let _div = n `div` (pow 10000 i) in
                let _mod = n `mod` (pow 10000 i) in
                convert4 3 _div ++ 
                    (if _div > 0 && i == 2 then "億"
                    else if _div > 0 && i == 1 then "万"
                    else "")
                    ++ cutting _mod (i-1) 
    in "金" ++ cutting num 2 ++ "円"

data Gengou = 
      Reiwa Int 
    | Heisei Int 
    | Showa Int
    | Taisho Int 
    | Meiji Int 
    | BeforeMeiji

instance Show Gengou where
    show (Reiwa y)  = "令和"++show y
    show (Heisei y) = "平成"++show y 
    show (Showa y) = "昭和"++show y 
    show (Taisho y) = "大正"++show y 
    show (Meiji y) = "明治"++show y 
    show BeforeMeiji = "明治以前"

christ2gengou :: Year -> Gengou
christ2gengou y | y > 2018 = Reiwa (y-2018)
                | y > 1988 = Heisei (y-1988)
                | y > 1925 = Showa (y-1925)
                | y > 1911 = Taisho (y-1911)
                | y > 1867 = Meiji (y-1867)
                | otherwise = BeforeMeiji

data Youbi = 
      Sun 
    | Mon 
    | Tue 
    | Wed 
    | Thu 
    | Fri 
    | Sat 
    deriving (Enum)

instance Show Youbi where
    show y = youbi!!(fromEnum y)
        where youbi = ["日", "月", "火", "水", "木", "金", "土"]

type Year = Int
type Day = Int

data Month = 
      None 
    | January 
    | Febrary 
    | March 
    | April
    | May 
    | June 
    | July 
    | August
    | September 
    | October 
    | November 
    | December
    deriving (Enum)

instance Show Month where
    show m = month!!(fromEnum m)
        where month = ["-1", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"]

newtype Date = Date (Year, Month, Day, Youbi)
instance Show Date where
    show (Date (y, m, d, yo))
        = show (christ2gengou y)++"年"++show m++"月"++show d++"日("++show yo++")"

year2days y = (y - 2000)*365 + (div (y - 1997) 4)

month2days y m  | leapYear y = if m > 2 then 1 + m2d m else m2d m
                | otherwise = m2d m

leapYear :: Year -> Bool
leapYear y =
    if y `mod` 400 == 0 then True
    else if y `mod` 100 == 0 then False
    else if y `mod` 4 == 0 then True
    else False


m2d n = sum (take (n-1) mdays)
    where mdays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30]


yokuJitu :: Youbi -> Youbi
yokuJitu yo = 
    toEnum((fromEnum yo + 1) `mod` 7) 

zenJitu :: Youbi -> Youbi
zenJitu yo = 
    toEnum((fromEnum yo + 6) `mod` 7) 


getYoubi d m y =
    if m < 3 then getYoubi d (m+12) (y-1) else
    (
        y + y`div`4 - y`div`100 + y`div`400 + (13*m+8)`div`5 + d + 7
    ) `mod` 7

getDays m y =
    if m == 2 then 
        if leapYear y then 
            29 
        else 
            28
    else 
        if m == 4 || m == 6 || m == 9 || m == 11 then
            30 
        else 
            31


month m y =
    let pad = getYoubi 1 m y in
    let days = getDays m y in
    let makeUp i = if i > 42 then "" -- 終了
        else 
            ( if i `mod` 7 == 1 then "\n" else "" ) -- 改行
            ++ 
            (
                if i - pad < 1 || days < i - pad then "   " ++ makeUp (i+1) -- 空白
                else " " ++ (if i - pad < 10 then " " else "") ++ show (i - pad) ++ makeUp (i+1)
            )
    in  
    "         "++ (if m < 10 then " " else "") ++ show m ++ "月        " ++ "\n" 
    ++ " 日 月 火 水 木 金 土" 
    ++ makeUp 1

split l [] list = reverse (l:list)
split l (c:r) list = 
    if c == '\n' then split "" r (l:list)
    else split (l++[c]) r list

combine [] [] [] = ""
combine (_a:a) (_b:b) (_c:c) =
    _a ++ "  " ++ _b ++ "  " ++ _c ++ "\n" ++ combine a b c 

year y = 
    "                                "
    ++ show y 
    ++ "\n\n"
    ++ combine
        (split "" (month 1 y) [])
        (split "" (month 2 y) [])
        (split "" (month 3 y) [])
    ++ "\n"
    ++ combine
        (split "" (month 4 y) [])
        (split "" (month 5 y) [])
        (split "" (month 6 y) [])
    ++ "\n"
    ++ combine
        (split "" (month 7 y) [])
        (split "" (month 8 y) [])
        (split "" (month 9 y) [])
    ++ "\n"
    ++ combine
        (split "" (month 10 y) [])
        (split "" (month 11 y) [])
        (split "" (month 12 y) [])
    ++ "\n"




data Nat = Zero | Succ Nat

plus :: Nat -> Nat -> Nat
plus Zero m = m
plus (Succ n) m = Succ (plus n m)

instance Show Nat where
    show Zero = "zero"
    show (Succ Zero) = "one"
    show (Succ (Succ Zero)) = "two"
    show (Succ (Succ (Succ Zero))) = "three"
    show (Succ (Succ (Succ (Succ Zero)))) = "four"


zero = Zero
one = Succ Zero
two = Succ one
three = Succ two
four = Succ three
five = Succ four
six = Succ five
seven = Succ six
eight = Succ seven
nine = Succ eight
ten = Succ nine
eleven = Succ ten
twelve = Succ eleven

times :: Nat -> Nat -> Nat
times Zero m = Zero
times (Succ n) m = plus (times n m) m

fact :: Nat -> Nat
fact Zero = Succ Zero
fact (Succ n) = times (Succ n) (fact n)

fib :: Nat -> Nat
fib Zero = Zero
fib (Succ Zero) = Succ Zero
fib (Succ (Succ n)) = plus (fib n) (fib (Succ n))

-- data Maybe Nat = Nothing | Just Nat
-- minus :: Nat -> Nat -> Maybe Nat
-- minus n Zero = Just n
-- minus Zero m = Nothing
-- minus (Succ n) (Succ m) = minus n m

instance Eq Nat where
    Zero == Zero = True
    Zero == Succ n = False
    Succ m == Zero = False
    Succ m == Succ n = m == n

instance Ord Nat where
    Zero <= Zero = True
    Zero <= Succ n = True
    Succ m <= Zero = False
    Succ m <= Succ n = m <= n

instance Num Nat where
    Zero + m = m
    (Succ n) + m = Succ (n + m)
    Zero * m = Zero
    (Succ n) * m = (n * m) + m
    abs m = m
    signum m = Succ Zero
    fromInteger 0 = Zero
    fromInteger n | n < 0 = Zero
                  | otherwise = Succ (fromInteger (n-1))
    m - Zero = m
    Zero - n = Zero
    Succ m - Succ n = m - n

infinity :: Nat
infinity = Succ infinity


minus :: Nat -> Nat -> Nat
minus n Zero = n
minus Zero m = error "minus"
minus (Succ n) (Succ m) = minus n m

divide :: Nat -> Nat -> (Nat, Nat)
divide _ Zero = error "divide by zero"
divide n m =
    if n < m then 
        (Zero, n)
    else 
        let (q, r) = divide (minus n m) m in
        (Succ q, r)


expt :: Nat -> Nat -> Nat
expt n Zero = Succ Zero
expt n (Succ m) = times n (expt n m)


-- type Place = Char
-- -- type Path = [Place]
-- newtype Path = Path [Place]
-- instance Show Path where
--     show (Path p) = "path " ++ showPath p
--         where showPath [] = "nothing"
--               showPath [p] = show p
--               showPath (p:ps) = showPath ps ++ " -> " ++ show p

-- search :: Place -> Place -> Path
-- search current_point goal_point = 
--     depth current_point goal_point (Path [])

-- depth :: Place -> Place -> Path -> Path
-- depth cp gp (Path path) | cp == gp = Path (gp:path)
--                         | otherwise = breadth (reachable cp) gp (Path (cp:path))

-- breadth :: [Place] -> Place -> Path -> Path
-- breadth [] gp path = Path []
-- breadth (cp:cps) gp path | null ans = breadth cps gp path
--                          | otherwise = Path ans
--     where Path ans = depth cp gp path


-- -- map
-- allPlaces = "ABCDEFGXYZ"

-- reachable :: Place -> [Place]
-- reachable cp = reachable' cp allPlaces

-- reachable' :: Place -> [Place] -> [Place]
-- reachable' cp [] = []
-- reachable' cp (np:nps) | transition (cp, np) = np:reachable' cp nps
--                        | otherwise = reachable' cp nps

-- transition :: (Place, Place) -> Bool
-- transition ('A', 'B') = True
-- transition ('A', 'C') = True
-- transition ('B', 'D') = True
-- transition ('B', 'G') = True
-- transition ('C', 'E') = True
-- transition ('D', 'F') = True
-- transition ('F', 'X') = True
-- transition ('G', 'X') = True
-- transition ('X', 'Z') = True
-- transition ('Y', 'Z') = True
-- transition ( s , t ) = False




type State = (Int, Int)
newtype Path = Path [State]
instance Show Path where
    show (Path p) = "path " ++ showPath p
        where showPath [] = "nothing"
              showPath [p] = show p
              showPath (p:ps) = showPath ps ++ " -> " ++ show p

search :: State -> State -> [Path]
search cs gs = depth cs gs (Path [])

depth :: State -> State -> Path -> [Path]
depth cs gs (Path path) | cs == gs = [Path (gs:path)]
                        | otherwise = breadth (reachable cs (cs:path)) gs (Path (cs:path))

breadth :: [State] -> State -> Path -> [Path]
breadth [] gs path = []
breadth (cs:css) gs path = depth cs gs path ++ breadth css gs path

reachable :: State -> [State] -> [State]
reachable cs css = empty3 cs css $ full3 cs css $ empty5 cs css
                    $ full5 cs css $ move35 cs css $ move53 cs css []

empty3,full3,empty5,full5,move35,move53 :: State -> [State] -> [State] -> [State]

empty3 (x, 0) css zs = zs
empty3 (x, y) css zs = if through (x, 0) css then zs else (x, 0):zs

full3 (x, 3) css zs = zs
full3 (x, y) css zs = if through (x, 3) css then zs else (x, 3):zs

empty5 (0, y) css zs = zs
empty5 (x, y) css zs = if through (0, y) css then zs else (0, y):zs

full5 (5, y) css zs = zs
full5 (x, y) css zs = if through (5, y) css then zs else (5, y):zs

move35 (x, 0) css zs = zs
move35 (x, y) css zs | z <= 5 = if through (z, 0) css then zs else (z, 0):zs
                     | otherwise = if through (5, z-5) css then zs else (5, z-5):zs
    where z = x+y

move53 (0, y) css zs = zs
move53 (x, y) css zs | z <= 3 = if through (0, z) css then zs else (0, z):zs
                     | otherwise = if through (z-3, 3) css then zs else (z-3, 3):zs
    where z = x+y

through:: (Eq a) => a -> [a] -> Bool
through s [] = False
through s (x:xs) = if s == x then True else through s xs
















type Queen = (Int, Int)

attack :: Queen -> Queen -> Bool
attack (x, y) (z, w) = 
    y == w || x - z == y - w || x - z == w - y

safe :: Queen -> [Queen] -> Bool
safe n [] = True
safe n (q:qs) = if attack n q then False else safe n qs

solve :: Int -> Int -> [Queen] -> Maybe [Queen]
solve 0 _ ans = Just ans
solve row 0 ans = Nothing
solve row col ans = maybe next return down
    where next = solve row (col-1) ans
          down = solve' (row-1) ((row, col):ans)

solve' :: Int -> [Queen] -> Maybe [Queen]
solve' row (q:qs) | safe q qs = solve row 8 (q:qs)
                  | otherwise = Nothing

queen = maybe (putStrLn "No answer") (display 8) (solve 8 8 [])

pos 0 n = ""
pos m 1 = " Q" ++ pos (m-1) 0
pos m n = " ." ++ pos (m-1) (n-1)

dispLine n (x, y) = putStrLn (pos n y)

display n [] = putStrLn ""
display n (x:xs) = dispLine n x >> display n xs
