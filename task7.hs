data Nat = Zero | Succ Nat

instance Show Nat where
    show Zero = "zero"
    show (Succ Zero) = "one"
    show (Succ (Succ Zero)) = "two"
    show (Succ (Succ (Succ Zero))) = "three"
    show (Succ (Succ (Succ (Succ Zero)))) = "four"
    show (Succ (Succ (Succ (Succ (Succ Zero))))) = "five"



plus :: Nat -> Nat -> Nat
plus Zero m = m
plus (Succ n) m = Succ (plus n m)

minus :: Nat -> Nat -> Nat
minus n Zero = n
minus Zero m = error "minus"
minus (Succ n) (Succ m) = minus n m

times :: Nat -> Nat -> Nat
times Zero m = Zero
times (Succ n) m = plus (times n m) m

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