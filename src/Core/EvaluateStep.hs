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
  | steps >= maxSteps config =
      error "Maximum number of reduction steps exceeded"
  | otherwise = case e of
      Var x -> case Map.lookup x env of
        Just ex ->
            let trace = EvalTrace steps e (Just $ "Variable lookup: " ++ x)
                (result, s, ts) = evaluateStep config ex env usedDefs (steps + 1) memo (trace:traces)
            in (result, s, ts)
        Nothing -> (e, steps, traces)
      App f a ->
        let (f', s1, t1) = evaluateStep config f env usedDefs steps memo traces
        in case f' of
          Lam x body ->
            let newExpr = substitute body x a
                trace = EvalTrace s1 e (Just $ "Beta reduction: " ++ show e)
            in evaluateStep config newExpr env usedDefs (s1 + 1)
                    (Map.insert e newExpr memo) (trace:t1)
          _ ->
            let (a', s2, t2) = evaluateStep config a env usedDefs s1 memo t1
            in (App f' a', s2, t2)
      _ -> (e, steps, traces)
