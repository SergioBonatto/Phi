module Types (
    InterpreterConfig(..),
    EvalTrace(..),
    MemoTable,
    EvalResult,
    Parser(..),
    Error(..),
    Result,
    ExprParser
) where

import qualified Data.Map as Map
import Expression (Expression)
import Error (Error(..))

data InterpreterConfig = InterpreterConfig {
    maxSteps :: Int,
    debug :: Bool,
    tracing :: Bool
}

data EvalTrace = EvalTrace {
    step :: Int,
    expr :: Expression,
    redex :: Maybe String
}

type MemoTable = Map.Map Expression Expression
type EvalResult = (Expression, Int, [EvalTrace])

class Parser p where
    parse :: p -> [String] -> Either Error (Expression, [String])

type Result = Either Error (Expression, [String])
type ExprParser = [String] -> Result
