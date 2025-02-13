module Evaluator (evaluate, evaluateWithTrace) where

import Expression (Expression)
import Environment (Env)
import Data.Set (Set)
import qualified Data.Map as Map
import EvaluateStep (evaluateStep)
import Types (InterpreterConfig(..), EvalTrace(..), MemoTable, EvalResult)

evaluate :: InterpreterConfig -> Expression -> Env -> Set String -> EvalResult
evaluate config expr env usedDefs =
    let (result, steps, traces) = evaluateWithTrace config expr env usedDefs Map.empty
    in (result, steps, if tracing config then traces else [])

evaluateWithTrace :: InterpreterConfig
                 -> Expression
                 -> Env
                 -> Set String
                 -> MemoTable
                 -> EvalResult
evaluateWithTrace config expr env usedDefs memo =
    case Map.lookup expr memo of
        Just cached -> (cached, 0, [])
        Nothing -> evaluateStep config expr env usedDefs 0 memo []
