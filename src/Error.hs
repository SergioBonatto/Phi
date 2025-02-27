module Error where

import Data.Text (Text)

data ParseError
    = UnexpectedToken Text
    | UnexpectedEOF
    | InvalidSyntax Text
    deriving (Show, Eq)

data EvalError
    = UnboundVariable Text
    | TypeError Text
    | MaxStepsExceeded Int
    | RuntimeError Text
    deriving (Show, Eq)

data InterpreterError
    = ParseErr ParseError
    | EvalErr EvalError
    deriving (Show, Eq)
