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