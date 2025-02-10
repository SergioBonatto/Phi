module EvaluateStep (evaluateStep) where

import Expression (Expression(..))
import Environment (Env)
import Substitution (substitute)
import qualified Data.Map as Map

evaluateStep :: Expression -> Env -> Int -> Int -> (Expression, Int)
evaluateStep e env steps maxSteps
  | steps >= maxSteps = error "Maximum number of reduction steps exceeded"
  | otherwise = case e of
      Var x -> case Map.lookup x env of
        Just ex -> evaluateStep ex env (steps + 1) maxSteps
        Nothing -> (e, steps)
      App f a ->
        let (f', s1) = evaluateStep f env steps maxSteps in
        case f' of
          Lam x body ->
            let newExpr = substitute body x a
            in evaluateStep newExpr env (s1 + 1) maxSteps
          _ ->
            let (a', s2) = evaluateStep a env s1 maxSteps
            in (App f' a', s2)
      _ -> (e, steps)
