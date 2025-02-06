module Evaluator (evaluate) where

import Expression (Expression(..))
import Environment (Env)
import qualified Data.Map as Map
import Data.Set (Set)

substitute :: Expression -> String -> Expression -> Expression
substitute (Var x) var val
    | x == var  = val
    | otherwise = Var x
substitute (Lam x body) var val
    | x == var  = Lam x body
    | otherwise = Lam x (substitute body var val)
substitute (App f a) var val =
    App (substitute f var val) (substitute a var val)

evaluate :: Expression -> Env -> Set String -> Int -> (Expression, Int)
evaluate expr env usedDefs maxSteps = go expr 0
  where
    go e steps
      | steps >= maxSteps = error "Maximum number of reduction steps exceeded"
      | otherwise =
          case e of
            Var x ->
              case Map.lookup x env of
                Just ex -> go ex (steps+1)
                Nothing -> (e, steps)
            App f a ->
              let (f', s1) = go f steps in
              case f' of
                Lam x body ->
                  let newExpr = substitute body x a
                  in go newExpr (s1+1)
                _ ->
                  let (a', s2) = go a (s1)
                  in (App f' a', s2)
            _ -> (e, steps)
