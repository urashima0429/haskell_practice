isLeapYear y =
    if y `mod` 400 == 0 then True
    else if y `mod` 100 == 0 then False
    else if y `mod` 4 == 0 then True
    else False


getYoubi d m y =
    if m < 3 then getYoubi d (m+12) (y-1) else
    (
        y + y`div`4 - y`div`100 + y`div`400 + (13*m+8)`div`5 + d + 7
    ) `mod` 7

getDays m y =
    if m == 2 then 
        if isLeapYear y then 
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
