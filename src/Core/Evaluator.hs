module Evaluator (
    evaluate,
    evaluateWithTrace,
    MemoKey,
    MemoValue,
    EvaluatorResult
) where

import Expression (Expression)
import Environment (Env)
import Data.Set (Set)
import qualified Data.Map as Map
import EvaluateStep (evaluateStep)
import Types (InterpreterConfig(..), EvalTrace(..), MemoTable)

type MemoKey = (Expression, Set String, Env)
type MemoValue = (Expression, Int, [EvalTrace])
type EvaluatorResult = (Expression, Int, [EvalTrace], MemoTable)

evaluate :: InterpreterConfig
        -> Expression
        -> Env
        -> Set String
        -> (Expression, Int, [EvalTrace])
evaluate config expr1 env usedDefs =
    let (result, steps, traces, _) =
            if memoization config
                then evaluateWithTrace config expr1 env usedDefs Map.empty
                else evaluateWithoutMemo config expr1 env usedDefs
    in (result, steps, if tracing config then traces else [])

evaluateWithoutMemo :: InterpreterConfig
                   -> Expression
                   -> Env
                   -> Set String
                   -> EvaluatorResult
evaluateWithoutMemo config expr1 env usedDefs =
    let initMemo = Map.empty
        initSteps = 0
        initTraces = []
    in evaluateStep config expr1 env usedDefs initSteps initMemo initTraces

evaluateWithTrace :: InterpreterConfig
                 -> Expression
                 -> Env
                 -> Set String
                 -> MemoTable
                 -> EvaluatorResult
evaluateWithTrace config expression env usedDefs memo =
    let key = (expression, usedDefs, env)
    in case Map.lookup key memo of
        Just (cached, steps, prevTraces) ->
            let trace = createTrace steps cached "Cache hit" (Map.size memo) "Memoization"
            in (cached, steps, trace:prevTraces, memo)

        Nothing ->
            let initSteps = 0
                initTraces = []
                (result, steps, traces, newMemo) =
                    evaluateStep config expression env usedDefs initSteps memo initTraces
            in if memoization config
                then (result, steps, traces, Map.insert key (result, steps, traces) newMemo)
                else (result, steps, traces, newMemo)

createTrace :: Int -> Expression -> String -> Int -> String -> EvalTrace
createTrace stepNum expr1 reason memUsage opt =
    EvalTrace {
        step = stepNum,
        expr = expr1,
        redex = Just reason,
        memoryUsage = memUsage,
        optimization = Just opt
    }
