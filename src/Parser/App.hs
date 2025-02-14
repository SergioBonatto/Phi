{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module App (AppParser(..)) where

import Expression (Expression(..))
import Types (Parser(..), ParserError(..))
import Var (VarParser(..))

data AppParser = AppParser

instance Monad m => Parser AppParser m where
    parse _ tokens = case tokens of
        [] -> return $ Left (UnexpectedEndOfInput "Expected expression")
        _  -> do
            parsed <- parse VarParser tokens
            case parsed of
                Left err -> return $ Left err
                Right (firstExpr, remainingTokens) ->
                    buildApplication firstExpr remainingTokens

    -- Implementação padrão do parseWithContext
    parseWithContext p tokens _ = parse p tokens

buildApplication :: Monad m => Expression -> [String] -> m (Either ParserError (Expression, [String]))
buildApplication acc [] = return $ Right (acc, [])
buildApplication acc (")":rest) = return $ Right (acc, ")":rest)
buildApplication acc tokens = do
    parsed <- parse VarParser tokens
    case parsed of
        Left err -> return $ Left err
        Right (nextExpr, remaining) ->
            buildApplication (App acc nextExpr) remaining
