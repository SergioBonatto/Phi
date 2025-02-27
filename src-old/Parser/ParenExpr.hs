{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module ParenExpr (ParenExprParser(..)) where

import Types (Parser(..), Result)
import Error (ParserError(..))
import App (AppParser(..))
import ExprTypes (ParserFunction)

newtype ParenExprParser = ParenExprParser ParserFunction

instance Monad m => Parser ParenExprParser m where
    parse (ParenExprParser exprParser) tokens = case tokens of
        ("(":rest) -> parseParens rest [] exprParser
        _ -> return $ Left $ UnexpectedToken "("
                     "Expected opening parenthesis"

    parseWithContext p tokens _ = parse p tokens

parseParens :: Monad m => [String] -> [String] -> ParserFunction -> m Result
parseParens [] _ _ = return $ Left $ UnclosedParenthesis
                     "Unclosed parenthesis at end of input"
parseParens (")":rest) accum exprParser = do
    result <- parse (AppParser exprParser) (reverse accum)
    case result of
        Left err -> return $ Left err
        Right (expr', []) -> return $ Right (expr', rest)
        Right (_, _) -> return $ Left $ UnexpectedToken ")"
                       "Unexpected tokens before closing parenthesis"
parseParens (t:ts) accum exprParser = parseParens ts (t:accum) exprParser
