module ParseApp (parseApp) where

import Expression (Expression(..))
import ParseCommon (parseExpr)

parseApp :: [String] -> (Expression, [String])
parseApp tokens =
    let (firstExpr, remainingTokens) = parseExpr tokens
    in parseApp' firstExpr remainingTokens

parseApp' :: Expression -> [String] -> (Expression, [String])
parseApp' expr [] = (expr, [])
parseApp' expr (")":ts) = (expr, (")":ts))
parseApp' expr ts =
    let (nextExpr, remainingTokens) = parseExpr ts
    in parseApp' (App expr nextExpr) remainingTokens
