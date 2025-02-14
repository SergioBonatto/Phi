{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module App (AppParser(..), buildApplication) where

import Expression (Expression(..))
import Types (Parser(..), Result, ExprParser)
import Var (VarParser(..))

data AppParser = AppParser ExprParser

instance Monad m => Parser AppParser m where
    parse (AppParser exprParser) tokens = do
        result <- parse VarParser tokens
        case result of
            Left err -> return $ Left err
            Right (firstExpr, remainingTokens) ->
                buildApplication firstExpr remainingTokens exprParser

    parseWithContext p tokens _ = parse p tokens

buildApplication :: Monad m =>
    Expression ->
    [String] ->
    ExprParser ->  -- Parser de expressões passado como parâmetro
    m Result
buildApplication acc [] _ = return $ Right (acc, [])
buildApplication acc (")":rest) _ = return $ Right (acc, ")":rest)
buildApplication acc ("(":tokens) exprParser = do
    case exprParser tokens of
        Left err -> return $ Left err
        Right (nextExpr, remaining) ->
            buildApplication (App acc nextExpr) remaining exprParser
buildApplication acc (tok:tokens) exprParser
    | tok `elem` ["λ", ".", "="] = return $ Right (acc, tok:tokens)
    | otherwise = do
        case exprParser (tok:tokens) of
            Left err -> return $ Left err
            Right (nextExpr, remaining) ->
                buildApplication (App acc nextExpr) remaining exprParser
