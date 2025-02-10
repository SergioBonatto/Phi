module Definition (definition) where

import Expression (Expression)
import Common (expr)
import Error (Error(..))

definition :: [String] -> Either Error (String, Expression)
definition ("let":name:"=":rest) = do
    (e, _) <- expr rest
    Right (name, e)
definition _ = Left InvalidLambdaSyntax
