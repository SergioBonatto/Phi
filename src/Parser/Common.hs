module Common (expr) where

import Expression (Expression(..))
import Error (Error(..))
import Types (Result, ExprParser)

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
parenExpr toks = do
    (appExpr, remainingTokens) <- app toks
    case remainingTokens of
        [] -> Left UnclosedParenthesis
        (")":rest) -> Right (appExpr, rest)
        _ -> Left UnclosedParenthesis

app :: ExprParser
app tokens = do
    (firstExpr, remainingTokens) <- expr tokens
    buildApplication firstExpr remainingTokens

buildApplication :: Expression -> [String] -> Result
buildApplication e [] = Right (e, [])
buildApplication e (")":ts) = Right (e, ")":ts)
buildApplication e ts = do
    (nextExpr, remainingTokens) <- expr ts
    buildApplication (App e nextExpr) remainingTokens
