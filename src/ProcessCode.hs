module ProcessCode (processCode) where

import Expression (Expression)
import Environment (Env)
import qualified Data.Map as Map
import Tokenize (tokenize)
import ParseDefinition (parseDefinition)
import ParseCommon (parseExpr)

processCode :: String -> (Expression, Env)
processCode code =
    let ls = filter (not . null) $ map strip $ lines code
    in go ls Map.empty Nothing
  where
    go [] env lastExpr = case lastExpr of
                           Nothing -> error "No expression to evaluate"
                           Just e  -> (e, env)
    go (l:ls') env _ =
        let tokens = tokenize l
        in case tokens of
             [] -> go ls' env Nothing
             ("let":_) -> let (name, expr) = parseDefinition tokens
                              env' = Map.insert name expr env
                          in go ls' env' (Just expr)
             _ -> let (expr, _) = parseExpr tokens
                  in go ls' env (Just expr)

    strip = unwords . words
