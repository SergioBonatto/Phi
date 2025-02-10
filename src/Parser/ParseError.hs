module ParseError (ParseError(..)) where

data ParseError
    = UnexpectedEndOfInput
    | UnexpectedToken String
    | UnclosedParenthesis
    | InvalidLambdaSyntax
    | EmptyExpression
    deriving (Show, Eq)
