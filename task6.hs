-- Gengou
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

-- Youbi
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


-- Month
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

type Year = Int
type Day = Int



newtype Date = Date (Year, Month, Day, Youbi)
instance Show Date where
    show (Date (y, m, d, yo))
        = show (christ2gengou y)++"年"++show m++"月"++show d++"日("++show yo++")"

leapYear :: Year -> Bool
leapYear y =
    if y `mod` 400 == 0 then True
    else if y `mod` 100 == 0 then False
    else if y `mod` 4 == 0 then True
    else False

yokuJitu :: Youbi -> Youbi
yokuJitu yo = 
    toEnum((fromEnum yo + 1) `mod` 7) 
    
zenJitu :: Youbi -> Youbi
zenJitu yo = 
    toEnum((fromEnum yo + 6) `mod` 7) 