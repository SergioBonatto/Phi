module ProcessCode (processCode) where

import Expression (Expression)
import Environment (Env)
import qualified Data.Map as Map
import Tokenize (tokenize)
import ParseDefinition (parseDefinition)
import ParseCommon (parseExpr)
import ParseError (ParseError(..))

stripLine :: String -> String
stripLine = unwords . words

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

processLine :: Either ParseError (Expression, Env, Maybe Expression) -> String -> Either ParseError (Expression, Env, Maybe Expression)
processLine acc line = do
    (_, env, lastExpr) <- acc
    let tokens = tokenize line
    processSingleLine tokens env lastExpr

processCode :: String -> Either ParseError (Expression, Env)
processCode code =
    let lines' = filter (not . null) $ map stripLine $ lines code
        result = foldl processLine (Right (undefined, Map.empty, Nothing)) lines'
    in case result of
        Left err -> Left err
        Right (expr, env, _) -> Right (expr, env)
