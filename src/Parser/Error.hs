module Error (
    Error(..),
    ParserError(..),
    EvalError(..),
    ConfigError(..),
    Position(..)
) where

data Position = Position {
    line :: Int,
    column :: Int
} deriving (Show, Eq)

data ParserError
    = UnexpectedEndOfInput String
    | UnexpectedToken String String
    | UnclosedParenthesis String
    | InvalidLambdaSyntax
    | EmptyExpression
    deriving (Show, Eq)

data EvalError
    = MaxStepsExceeded Int
    | UnboundVariable String
    | TypeError String
    | RuntimeError String
    deriving (Show, Eq)

data ConfigError
    = InvalidStepLimit Int
    | InvalidLogLevel String
    | InvalidExtension String
    deriving (Show, Eq)

data Error
    = ParserError ParserError
    | EvalError EvalError
    | ConfigError ConfigError
    deriving (Show, Eq)
