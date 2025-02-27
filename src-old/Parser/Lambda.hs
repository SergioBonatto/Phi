{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Lambda (LambdaParser(..)) where

import Expression (Expression(..))
import Types (Parser(..))
import Error (ParserError(..))
import Var (VarParser(..))

data LambdaParser = LambdaParser

instance Monad m => Parser LambdaParser m where
    parse _ [] = return $ Left (UnexpectedEndOfInput "Expected lambda expression")
    parse _ (var:".":rest) = do
        parsed <- parse VarParser rest
        case parsed of
            Left err -> return $ Left err
            Right (body, remaining) -> return $ Right (Lam var body, remaining)
    parse _ _ = return $ Left (UnexpectedToken "lambda" "Expected variable and dot")

    parseWithContext p tokens _ = parse p tokens
