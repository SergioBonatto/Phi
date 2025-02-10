module Error (Error(..)) where

data Error
    = UnexpectedEndOfInput
    | UnexpectedToken String
    | UnclosedParenthesis
    | InvalidLambdaSyntax
    | EmptyExpression
    deriving (Show, Eq)
