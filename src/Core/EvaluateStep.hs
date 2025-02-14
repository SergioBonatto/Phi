module EvaluateStep (evaluateStep) where

import Expression (Expression(..))
import Environment (Env)
import Substitution (substitute)
import qualified Data.Map as Map
import Data.Set (Set)
import Types (InterpreterConfig(..), EvalTrace(..), MemoTable, EvalResult)
import Error()

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
        -- Caso para variáveis
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

        -- Caso para aplicações
        App e1 e2 ->
            let (e1', s1, t1, m1) = evaluateStep config e1 env usedDefs steps memo traces
            in case e1' of
                Lam x body ->
                    let trace = EvalTrace {
                            step = s1,
                            expr = e,
                            redex = Just "Beta reduction",
                            memoryUsage = Map.size m1,
                            optimization = Nothing
                        }
                        reduced = substitute body x e2
                    in evaluateStep config reduced env usedDefs (s1 + 1) m1 (trace:t1)
                _ ->
                    let (e2', s2, t2, m2) = evaluateStep config e2 env usedDefs s1 m1 t1
                    in (App e1' e2', s2, t2, m2)

        -- Caso para lambda
        Lam x body ->
            let (body', s, t, m) = evaluateStep config body env usedDefs steps memo traces
            in (Lam x body', s, t, m)
