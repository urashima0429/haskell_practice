-- Fun
doubleMe x = x * 2
doubleUs x y = x * 2 + y * 2
doubleSmallerNumber x y =
  if x < y then x * 2 else y * 2

-- Append
nn = [1,2,3] ++ [4,5,6]
hlw = "Hello" ++ " " ++ "World"
wood = ['w', 'o', 'o', 'd']

-- Cons
asc = 'A':' ':"SMALL CAT"
nnn = 0:1:2:3:4:5:6:[]

-- geti
u =  "Urashima Taro" !! 0 -- U

-- list nesting
ll = [5]:[3:[4]]

-- list comparison
t = [3,4,1] < [3,4,5] -- t
f = [3,4,1] < [3,4]   -- f

-- list builtin fun
li = [5,4,3,4]
hed = head li -- 5
lst = last li -- 4
ini = init li -- [5,4,3]
tal = tail li -- [4,3,4]
len = length li -- 4
nul = null li -- false
rev = reverse li --[4,3,4,5]
tak = take 2 li -- [4,3]
drp = drop 2 li -- [4,5]
mkm = maximum li -- [5]
mnm = minimum li --[3]
sumi = sum li -- 16
proi = product li -- 240
elm = 3 `elem` li -- true

-- range
li0 = [2, 4..20] -- [2,4,6,8,10,12,14,16,18,20] 
li1 = ['a', 'c'..'z'] -- "acegikmoqsuwy" 
li2 = take 4 [7,11..] -- [7,11,15,19]
li3 = take 7 (cycle [1,2,3]) -- [1,2,3,1,2,3,1] 
li4 = take 3 (repeat 5) -- [5,5,5]
li5 = replicate 3 7 -- [7,7,7]

--list comprehension
li6 = [x*x | x <- [1..5]] -- [1,4,9,16,25]
li7 = [x | x <- [50..100], x `mod` 7 == 3]
  -- [52,59,66,73,80,87,94] 
goodbad xs = [if x `mod` 7 == 3 then "good" else "bad" | x <- xs, odd x]
li8 = [x + y | x <- [1,2,3] , y <- [10,20,30]]
  -- [11,21,31,12,22,32,13,23,33]
length' xs = sum [1 | _ <- xs]
xxs = [ [1,2,3],[4,5,6],[7,8,9] ]
li9 = [ [x | x <- xs, even x] | xs <- xxs]
  -- [[2],[4,6],[8]] 

-- tuple
tp0 = (1,False)
first = fst tp0 -- 1
second = snd tp0 -- False
tp1 = zip [1..] ["one","two","three"]
  -- [(1,"one"),(2,"two"),(3,"three")] 


-- :t 'a'  -- Char
-- :t True  -- Bool
-- :t "Hello" -- [Char]

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n = product [1..n]
 