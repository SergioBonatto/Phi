module Evaluator (
    evaluate
) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.Except
import Control.Monad.State
import qualified Types as T
import Error (EvalError(..))
import Expression (Expression(..))
import Environment (Env)

-- Estado da avaliação com trace
data EvalState = EvalState
    { envState  :: Env
    , stepCount :: Int
    , evalTrace :: [T.EvalTrace]
    }

type EvalM a = ExceptT EvalError (State EvalState) a

-- Incrementa o contador de passos
incrementStep :: EvalM ()
incrementStep = modify $ \s -> s { stepCount = stepCount s + 1 }

-- Versão corrigida para usar o construtor T.EvalTrace corretamente
addTrace :: String -> Expression -> EvalM ()
addTrace msg expr = do
    steps <- gets stepCount
    let traceMsg = Just (msg ++ " (" ++ show expr ++ ")")
    modify $ \s -> s { evalTrace = T.EvalTrace steps expr traceMsg 0 Nothing : evalTrace s }

-- Função de substituição: substitui a variável 'x' pela expressão 'value' em 'expr'
substitute :: String -> Expression -> Expression -> Expression
substitute x value expr = case expr of
    Var y -> if y == x then value else Var y
    Lam y body ->
        if y == x
        then Lam y body -- Variável ligada, não substitui no corpo
        else Lam y (substitute x value body)
    App f arg -> App (substitute x value f) (substitute x value arg)

-- Avalia uma expressão
eval :: Expression -> EvalM Expression
eval expr = do
    incrementStep
    addTrace "Avaliando" expr
    case expr of
        Var x -> do
            env <- gets envState
            case Map.lookup x env of
                Just e  -> eval e  -- Avaliar a variável encontrada no ambiente
                Nothing -> throwError $ UnboundVariable x
        Lam x body -> return $ Lam x body
        App f arg -> do
            f' <- eval f
            arg' <- eval arg  -- Avaliar o argumento
            case f' of
                Lam x body -> do
                    -- Substituição léxica em vez de usar o ambiente
                    let newBody = substitute x arg' body
                    addTrace ("Substituindo " ++ x) newBody
                    eval newBody
                _ -> throwError $ TypeError "Expected a function in application"

-- Nova função evaluate que recebe o ambiente e o conjunto de definições usadas
evaluate :: T.InterpreterConfig -> Expression -> Env -> Set.Set String -> (Expression, Int, [T.EvalTrace])
evaluate config expr env _ =
    let initialState = EvalState env 0 []
        (resultE, finalState) = runState (runExceptT (eval expr)) initialState
    in case resultE of
         Left err -> error (show err)
         Right val ->
           if stepCount finalState > T.maxSteps config
              then error ("Maximum steps exceeded: " ++ show (T.maxSteps config))
              else (val, stepCount finalState, reverse (evalTrace finalState))
