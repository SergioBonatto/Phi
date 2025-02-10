module App (AppParser(..)) where

import Expression (Expression(..))
import Types (Result, Parser(..), Error(..))
import Var (VarParser(..))

data AppParser = AppParser

instance Parser AppParser where
    parse _ tokens = case tokens of
        [] -> Left UnexpectedEndOfInput
        _  -> do
            (firstExpr, remainingTokens) <- parse VarParser tokens
            buildApplication firstExpr remainingTokens

buildApplication :: Expression -> [String] -> Result
buildApplication acc [] = Right (acc, [])
buildApplication acc (")":rest) = Right (acc, ")":rest)
buildApplication acc tokens = do
    (nextExpr, remaining) <- parse VarParser tokens
    buildApplication (App acc nextExpr) remaining
