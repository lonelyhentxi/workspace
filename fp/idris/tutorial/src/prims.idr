module Prims

x : Int
x = 42

foo : String
foo = "Sausage machine"

bar : Char
bar = 'Z'

quux : Bool
quux = False

plus' : Nat -> Nat -> Nat
plus' Z y = y
plus' (S k) y = S (plus k y)

mult' : Nat -> Nat -> Nat
mult' Z y = Z
mult' (S k) y = plus y (mult k y)

reverse' : List a -> List a
reverse' xs =  revAcc [] xs where
    revAcc : List a -> List a -> List a
    revAcc acc [] = acc
    revAcc acc (x :: xs) = revAcc (x :: acc) xs

foo' : Int -> Int
foo' x = case isLT of 
        YES => x*2
        NO => x*4
    where
        data MyLT = YES | NO
        
        isLT: MyLT
        isLT = if x<20 then YES else NO

-- 一般 where 从句中定义的函数和其它顶层函数一样，都需要类型声明
-- 函数 f 的类型声明可以在以下情况中省略
-- 1. f 出现在顶层定义的右边
-- 2. f 的类型完全可以通过其首次应用来确定

even' : Nat -> Bool
even' Z = True
even' (S k) = odd' k where
    odd' Z = False
    odd' (S k) = even' k