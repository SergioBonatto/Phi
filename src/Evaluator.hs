module Evaluator (
    evaluate
) where

import Types
import Error
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.Except
import Control.Monad.State

type EvalM a = ExceptT EvalError (State EvalState) a

data EvalState = EvalState
    { envState :: Env
    , stepCount :: Int
    , memoTable :: MemoTable
    }

evaluate :: InterpreterConfig -> Expression -> Either EvalError Expression
evaluate config expr =
    let initialState = EvalState Map.empty 0 Map.empty
        (result, finalState) = runState (runExceptT (eval expr)) initialState
    in if stepCount finalState > maxSteps config
           then Left $ MaxStepsExceeded (maxSteps config)
           else result

eval :: Expression -> EvalM Expression
eval (Var x) = do
    env <- gets envState
    case Map.lookup x env of
        Just e  -> return e
        Nothing -> throwError $ UnboundVariable x
eval (Lam x body) = return $ Lam x body
eval (App f arg) = do
    f' <- eval f
    case f' of
        Lam x body -> do
            arg' <- eval arg
            local (Map.insert x arg') (eval body)
        _ -> throwError $ TypeError "Expected a function in application"
