module Common (expr) where

import Expression (Expression(..))
import Error (Error(..))
import Types (ExprParser, Parser(..))
import ParenExpr (ParenExprParser(..))

expr :: ExprParser
expr [] = Left UnexpectedEndOfInput
expr (tok:toks)
    | tok == "λ" = lambda toks
    | tok == "(" = parenExpr toks
    | tok == ")" || tok == "." || tok == "=" =
        Left $ UnexpectedToken tok
    | otherwise = Right (Var tok, toks)

lambda :: ExprParser
lambda (var:".":rest) = do
    (body, remainingTokens) <- expr rest
    Right (Lam var body, remainingTokens)
lambda _ = Left InvalidLambdaSyntax

parenExpr :: ExprParser
parenExpr = parse ParenExprParser
