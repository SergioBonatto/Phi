module Common (expr) where

import Expression (Expression(..))
import Error (ParserError(..))
import Types (ExprParser, Parser(..))
import ParenExpr (ParenExprParser(..))
import Data.Either (either)

expr :: ExprParser
expr [] = Left (UnexpectedEndOfInput "Expected expression")
expr (tok:toks)
    | tok == "λ" = lambda toks
    | tok == "(" = parenExpr toks
    | tok == ")" || tok == "." || tok == "=" =
        Left (UnexpectedToken tok "Expected expression")
    | otherwise = Right (Var tok, toks)

lambda :: ExprParser
lambda (var:".":rest) = do
    (body, remainingTokens) <- expr rest
    Right (Lam var body, remainingTokens)
lambda _ = Left InvalidLambdaSyntax

parenExpr :: ExprParser
parenExpr tokens = either Left id $ parse ParenExprParser tokens
