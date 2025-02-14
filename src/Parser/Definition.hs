module Definition (definition) where

import Expression (Expression)
import Common (expr)
import Error (Error(..), ParserError(..))

definition :: [String] -> Either Error (String, Expression)
definition ("let":name:"=":rest) = do
    case expr rest of
        Left err -> Left $ ParserError err
        Right (e, _) -> Right (name, e)
definition _ = Left $ ParserError InvalidLambdaSyntax
