{-
farmer(f), wolf(w), sheep(s), cabbage(c)

状態:(
    Bool f,
    Bool w, 
    Bool s,
    Bool c
    )

操作:
    if (f){
        f = False
        w, s, c のいずれかをFalseにしてよい（しなくてもよい）
    } else {
        f = True
        w, s, c のいずれかをTrueにしてよい（しなくてもよい）
    }

条件:以下の状態になれば探索打ち切り
    (f, w, s, c) ==
           (False, True, True, _)
        || (False, _, True, True)
        || (True, False, False, _)
        || (True, _, False, False)

初期状態 : (False, False, False, False)
終了状態 : (True, True, True, True)

-}

-- f, w, s, cがそれぞれ南の岸にいるか
type State = (Bool, Bool, Bool, Bool)
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
reachable cs css = move_w cs css $ move_s cs css $ move_c cs css $ not_move cs css []

move_w, move_s, move_c, not_move :: State -> [State] -> [State] -> [State]

-- 狼を船で運ぶ操作
move_w (f, w, s, c) css zs = 
    if f /= w || through ns css || isBadState ns then zs else ns:zs 
    where ns = (not f, not w, s, c)

-- 羊を船で運ぶ操作
move_s (f, w, s, c) css zs = 
    if f /= s || through ns css || isBadState ns then zs else ns:zs 
    where ns = (not f, w, not s, c)

-- キャベツを船で運ぶ操作
move_c (f, w, s, c) css zs = 
    if f /= c || through ns css || isBadState ns then zs else ns:zs 
    where ns = (not f, w, s, not c)

-- 農夫のみ船で移動する操作
not_move (f, w, s, c) css zs = 
    if through ns css || isBadState ns then zs else ns:zs 
    where ns = (not f, w, s, c)

-- 既に探索済みの状態　探索打ち切り
through :: (Eq a) => a -> [a] -> Bool
through s [] = False
through s (x:xs) = if s == x then True else through s xs

-- 禁止された状態　探索打ち切り
isBadState :: State -> Bool
isBadState (False, True, True, _) = True
isBadState (False, _, True, True) = True
isBadState (True, False, False, _) = True
isBadState (True, _, False, False) = True
isBadState _ = False

-- 想定の実行
-- first_state = (False, False, False, False)  -- 初期状態 : すべて北岸
-- goal_state = (True, True, True, True)       -- 終了状態 : すべて南岸
-- search first_state goal_state
-- => [path (False,False,False,False) -> (True,False,True,False) -> (False,False,True,False) -> (True,True,True,False) -> (False,True,False,False) -> (True,True,False,True) -> (False,True,False,True) -> (True,True,True,True),path (False,False,False,False) -> (True,False,True,False) -> (False,False,True,False) -> (True,False,True,True) -> (False,False,False,True) -> (True,True,False,True) -> (False,True,False,True) -> (True,True,True,True)]