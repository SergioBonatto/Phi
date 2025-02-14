{-# LANGUAGE FlexibleContexts #-}

module Common (expr) where

import Expression (Expression(..))
import Error (ParserError(..))
import Types (ExprParser, Parser(..), Result)
import ParenExpr (ParenExprParser(..))
import Data.Functor.Identity (Identity(..))
import Data.Either()

expr :: ExprParser
expr [] = Left (UnexpectedEndOfInput "Expected expression")
expr (tok:toks)
    | tok == "λ" = lambda toks expr
    | tok == "(" = runParser (ParenExprParser expr) (tok:toks)
    | tok == ")" || tok == "." || tok == "=" =
        Left (UnexpectedToken tok "Expected expression")
    | otherwise = Right (Var tok, toks)

lambda :: [String] -> ExprParser -> Result
lambda (var:".":rest) exprParser = do
    (body, remainingTokens) <- exprParser rest
    Right (Lam var body, remainingTokens)
lambda _ _ = Left InvalidLambdaSyntax

runParser :: Parser p Identity => p -> [String] -> Result
runParser p tokens = runIdentity $ parse p tokens
