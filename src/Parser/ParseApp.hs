module ParseApp (AppParser(..)) where

import Expression (Expression(..))
import ParseTypes (ParserResult, Parser(..), ParseError(..))
import ParseVar (VarParser(..))

data AppParser = AppParser

instance Parser AppParser where
    parse _ tokens = case tokens of
        [] -> Left UnexpectedEndOfInput
        _  -> do
            (firstExpr, remainingTokens) <- parse VarParser tokens
            buildApplication firstExpr remainingTokens

buildApplication :: Expression -> [String] -> ParserResult
buildApplication acc [] = Right (acc, [])
buildApplication acc (")":rest) = Right (acc, ")":rest)
buildApplication acc tokens = do
    (nextExpr, remaining) <- parse VarParser tokens
    buildApplication (App acc nextExpr) remaining
