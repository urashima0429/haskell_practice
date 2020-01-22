import Data.Char

type Position = (Int, Int)

getInteger :: String -> Int -> (Int, String)
getInteger [] x = (x, [])
getInteger (c:cs) x | isDigit c = getInteger cs (10*x + digitToInt c)
                    | otherwise = (x, cs)

makePosition :: String -> Position
makePosition s1 = (x, y)
    where (x, s2) = getInteger s1 0
          (y, s3) = getInteger s2 0

valid :: Position -> Bool
valid (x, y) = (0 <= x && x <= 2) && (0 <= y && y <= 2)

getPosition :: IO Position
getPosition = do putStr "please input: "
                 str <- getLine
                 let (x,y) = makePosition str
                 validate (x, y)
    where validate (x, y) | valid (x, y) = return (x, y)
                          | otherwise = getPosition
-- 初期フィールド
initField = "---------"

-- フィールドの表示
printField :: String -> IO ()
printField [] = putStrLn "" >> return ()
printField l = putStrLn a >> printField b
    where (a, b) = splitAt 3 l

-- フィールドの更新
updateField :: String -> Char -> Position -> String
updateField f s (x,y) = (a ++ s:(tail b))
    where (a, b) = splitAt (3*y+x) f

-- 勝利判定
judgeLine :: Char -> Char -> Char -> Bool
judgeLine a b c = (a == b) && (b == c) && (c /= '-')

judge :: String -> Bool
judge f = 
       judgeLine c0 c1 c2
    || judgeLine c3 c4 c5
    || judgeLine c6 c7 c8
    || judgeLine c0 c3 c6
    || judgeLine c1 c4 c7
    || judgeLine c2 c5 c8
    || judgeLine c0 c4 c8
    || judgeLine c2 c4 c6
    where (c0:c1:c2:c3:c4:c5:c6:c7:c8:_) = f

-- 既に埋まっていれば True を返す
checkPosition :: String -> Position -> Bool
checkPosition f (x,y) = f !! (3*y+x) /= '-'

-- 終了判定
terminater [] = True
terminater (s:l) = s /= '-' && terminater l

-- o側のターン処理
-- 既に埋まっているフィールドが入力されればやり直し
turnCircle f = 
    putStrLn "turn o" 
    >> getPosition 
    >>= (\ p -> 
        if checkPosition f p 
            then putStrLn "!! already used !!" >> return f
            else return (updateField f 'o' p)
        )
    >>= (\ f' -> printField f' >>
            if f' == f then turnCircle f'
            else if judge f' then putStrLn "!! Winner o !!" >> return () 
            else if terminater f' then putStrLn "!! End in a draw !!" >> return ()
            else turnCross f'
        )

-- x側のターン処理
turnCross f =
    putStrLn "turn x" 
    >> getPosition
    >>= (\ p -> 
        if checkPosition f p
            then putStrLn "!! already used !!" >> return f
            else return (updateField f 'x' p)
        )
    >>= (\ f' -> printField f' >> 
            if f' == f then turnCross f' 
            else if judge f' then putStrLn "!! Winner x !!" >> return ()
            else if terminater f' then putStrLn "!! End in a draw !!" >> return ()
            else turnCircle f'
        )

gameStart = turnCircle initField
