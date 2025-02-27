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
eval expr = do
    incrementStep
    addTrace "Evaluating" expr
    case expr of
        Var x -> do
            env <- gets envState
            case Map.lookup x env of
                Just e  -> eval e
                Nothing -> return (Var x)
        Lam x body -> return $ Lam x body
        App f arg -> do
            f' <- eval f
            case f' of
                Lam x body -> do
                    arg' <- eval arg
                    let newBody = substitute x arg' body
                    addTrace ("Substituting " ++ x) newBody
                    eval newBody
                _ -> do
                    arg' <- eval arg
                    return $ App f' arg'
