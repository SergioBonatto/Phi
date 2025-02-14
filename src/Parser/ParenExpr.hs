{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module ParenExpr (ParenExprParser(..)) where

import Types (Parser(..))
import Error (ParserError(..))
import App (AppParser(..))

data ParenExprParser = ParenExprParser

instance Monad m => Parser ParenExprParser m where
    parse _ toks = do
        parsed <- parse AppParser toks
        case parsed of
            Left err -> return $ Left err
            Right (appExpr, remainingTokens) ->
                case remainingTokens of
                    [] -> return $ Left (UnclosedParenthesis "Missing closing parenthesis")
                    (")":rest) -> return $ Right (appExpr, rest)
                    _ -> return $ Left (UnclosedParenthesis "Missing closing parenthesis")

    parseWithContext p tokens _ = parse p tokens
