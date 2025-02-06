module ProcessCode (processCode) where

import Expression (Expression)
import Environment (Env)
import qualified Data.Map as Map
import Tokenize (tokenize)
import ParseDefinition (parseDefinition)
import ParseCommon (parseExpr)
import ParseError (ParseError(..))

processCode :: String -> Either ParseError (Expression, Env)
processCode code =
    let ls = filter (not . null) $ map strip $ lines code
    in go ls Map.empty Nothing
  where
    go [] env lastExpr = case lastExpr of
                           Nothing -> Left EmptyExpression
                           Just e  -> Right (e, env)
    go (l:ls') env lastExpr =
        let tokens = tokenize l
        in case tokens of
             [] -> go ls' env lastExpr
             ("let":_) -> do
                 (name, expr) <- parseDefinition tokens
                 let env' = Map.insert name expr env
                 go ls' env' (Just expr)
             _ -> do
                 (expr, _) <- parseExpr tokens
                 go ls' env (Just expr)

    strip = unwords . words
