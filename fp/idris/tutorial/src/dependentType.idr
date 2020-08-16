-- 依赖类型
-- 一等类型
-- 在 idris 中，类型是一等的，即它们可以像其它的语言构造那样被计算和操作

isSingleton : Bool -> Type
isSingleton True = Nat
isSingleton False = List Nat

mkSingle : (x : Bool) -> isSingleton x
mkSingle True = 0
mkSingle False = []

sum' : (single: Bool) -> isSingleton single -> Nat
sum' True x = x
sum' False [] = 0
sum' False (x::xs) = x + sum' False xs

-- sum' False 1
-- will return
-- When checking an application of function Main.sum:
-- isSingleton False is not a numeric type