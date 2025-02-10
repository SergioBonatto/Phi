module ProcessSingleLine (processSingleLine) where

import Expression (Expression)
import Environment (Env)
import qualified Data.Map as Map
import ParseDefinition (parseDefinition)
import ParseCommon (parseExpr)
import ParseError (ParseError(..))

processSingleLine :: [String] -> Env -> Maybe Expression -> Either ParseError (Expression, Env, Maybe Expression)
processSingleLine [] env lastExpr = case lastExpr of
    Nothing -> Left EmptyExpression
    Just e -> Right (e, env, Just e)
processSingleLine tokens env lastExpr = case tokens of
    [] -> Right (undefined, env, lastExpr)
    ("let":_) -> do
        (name, expr) <- parseDefinition tokens
        let env' = Map.insert name expr env
        Right (expr, env', Just expr)
    _ -> do
        (expr, _) <- parseExpr tokens
        Right (expr, env, Just expr)
