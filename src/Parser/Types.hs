module Types (
    ExprParser,
    Result,
    Parser(..),
    Error(..)
) where

import Expression (Expression)
import Error (Error(..))

type Result = Either Error (Expression, [String])
type ExprParser = [String] -> Result

class Parser p where
    parse :: p -> [String] -> Result
