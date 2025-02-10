module Expression (Expression(..)) where

data Expression
    = Var String
    | Lam String Expression
    | App Expression Expression
    deriving (Eq)

instance Show Expression where
    show (Var x)     = x
    show (Lam x e)   = "λ" ++ x ++ ". " ++ show e
    show (App f a)   = "(" ++ show f ++ " " ++ show a ++ ")"
