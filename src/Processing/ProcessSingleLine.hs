module ProcessSingleLine (processSingleLine) where

import Expression (Expression)
import Environment (Env)
import qualified Data.Map as Map
import Definition (definition)
import Common (expr)
import Error (Error(..), ParserError(..))

processSingleLine :: [String] -> Env -> Maybe Expression -> Either Error (Expression, Env, Maybe Expression)
processSingleLine [] env lastExpr = Right (undefined, env, lastExpr)
processSingleLine tokens env _ = case tokens of
    ("let":_) -> do
        (name, expr) <- definition tokens
        let env' = Map.insert name expr env
        Right (expr, env', Just expr)
    _ -> case expr tokens of
        Left err -> Left $ ParserError err
        Right (parsedExpr, _) -> Right (parsedExpr, env, Just parsedExpr)
