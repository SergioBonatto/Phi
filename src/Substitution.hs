module Substitution (substitute) where

import Expression (Expression(..))

substitute :: Expression -> String -> Expression -> Expression
substitute (Var x) var val
    | x == var  = val
    | otherwise = Var x
substitute (Lam x body) var val
    | x == var  = Lam x body
    | otherwise = Lam x (substitute body var val)
substitute (App f a) var val =
    App (substitute f var val) (substitute a var val)
