module ProcessCode (processCode) where

import Expression (Expression)
import Environment (Env)
import qualified Data.Map as Map
import ParseError (ParseError)
import StripLine (stripLine)
import ProcessLine (processLine)

processCode :: String -> Either ParseError (Expression, Env)
processCode code =
    let lines' = filter (not . null) $ map stripLine $ lines code
        result = foldl processLine (Right (undefined, Map.empty, Nothing)) lines'
    in case result of
        Left err -> Left err
        Right (expr, env, _) -> Right (expr, env)
