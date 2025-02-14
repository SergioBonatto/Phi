module EvaluateStep (evaluateStep) where

import Expression (Expression(..))
import Environment (Env)
import Substitution (substitute)
import qualified Data.Map as Map
import Data.Set (Set)
import Types (InterpreterConfig(..), EvalTrace(..), MemoTable, EvalResult)
import Error (Error(..))

evaluateStep :: InterpreterConfig
             -> Expression
             -> Env
             -> Set String
             -> Int
             -> MemoTable
             -> [EvalTrace]
             -> EvalResult
evaluateStep config e env usedDefs steps memo traces
    | steps >= maxSteps config = error "Maximum number of reduction steps exceeded"
    | otherwise = case e of
        Var x -> case Map.lookup x env of
            Just ex ->
                let trace = EvalTrace {
                        step = steps,
                        expr = e,
                        redex = Just $ "Variable lookup: " ++ x,
                        memoryUsage = Map.size memo,
                        optimization = Nothing
                    }
                    (result, s, ts, m) = evaluateStep config ex env usedDefs (steps + 1) memo (trace:traces)
                in (result, s, ts, m)
            Nothing -> (e, steps, traces, memo)
