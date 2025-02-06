module ParseDefinition (parseDefinition) where

import Expression (Expression)
import ParseCommon (parseExpr)

parseDefinition :: [String] -> (String, Expression)
parseDefinition ("let":name:"=":rest) =
    let (expr, _) = parseExpr rest
    in (name, expr)
parseDefinition _ = error "Invalid syntax in let definition"
