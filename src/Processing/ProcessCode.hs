module ProcessCode (processCode) where

import Expression (Expression)
import Environment (Env)
import qualified Data.Map as Map
import Error (Error(..), ParserError(..))  -- Adicionar ParserError
import StripLine (stripLine)
import ProcessLine (processLine)

processCode :: String -> Either Error (Expression, Env)
processCode code =
    let nonEmptyLines = filter (not . null . words) $ map stripLine $ lines code
        initState = Right (undefined, Map.empty, Nothing)
        result = foldl processLine initState nonEmptyLines
    in case result of
        Left err -> Left err
        Right (_, env, Just lastExpr) -> Right (lastExpr, env)
        Right (_, _, Nothing) -> Left $ ParserError EmptyExpression  -- Adicionar ParserError
