module ParseApp (parseParenApplication) where

import Expression (Expression(..))
import ParseCommon (parseExpr)
import ParseTypes (ParserResult)

parseParenApplication :: [String] -> ParserResult
parseParenApplication tokens = do
    (firstExpr, remainingTokens) <- parseExpr tokens
    buildApplication firstExpr remainingTokens

buildApplication :: Expression -> [String] -> ParserResult
buildApplication expr [] = Right (expr, [])
buildApplication expr (")":ts) = Right (expr, (")":ts))
buildApplication expr ts = do
    (nextExpr, remainingTokens) <- parseExpr ts
    buildApplication (App expr nextExpr) remainingTokens
