even'' : Nat -> Bool
even'' Z = True
even'' (S k) = ?even_rhs

-- :t even_rhs
-- k : Nat
--------------
-- even_rhs : Bool

main : IO ()
main = putStrLn ?greeting

-- :l hole
-- :t greeting
--------------
-- greeting : String