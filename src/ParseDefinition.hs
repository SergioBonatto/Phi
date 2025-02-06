module ParseDefinition (parseDefinition) where

import Expression (Expression)
import ParseCommon (parseExpr)
import ParseError (ParseError(..))

parseDefinition :: [String] -> Either ParseError (String, Expression)
parseDefinition ("let":name:"=":rest) = do
    (expr, _) <- parseExpr rest
    Right (name, expr)
parseDefinition _ = Left InvalidLambdaSyntax
