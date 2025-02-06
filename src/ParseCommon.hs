module ParseCommon (parseExpr) where

import Expression (Expression(..))
import ParseError (ParseError(..))
import ParseTypes (ParserResult, ExprParser)

parseExpr :: ExprParser
parseExpr [] = Left UnexpectedEndOfInput
parseExpr (tok:toks)
    | tok == "λ" = parseLambda toks
    | tok == "(" = parseParenExpr toks
    | tok == ")" || tok == "." || tok == "=" =
        Left $ UnexpectedToken tok
    | otherwise = Right (Var tok, toks)

parseLambda :: ExprParser
parseLambda (var:".":rest) = do
    (body, remainingTokens) <- parseExpr rest
    Right (Lam var body, remainingTokens)
parseLambda _ = Left InvalidLambdaSyntax

parseParenExpr :: ExprParser
parseParenExpr toks = do
    (appExpr, remainingTokens) <- parseApp toks
    case remainingTokens of
        [] -> Left UnclosedParenthesis
        (")":rest) -> Right (appExpr, rest)
        _ -> Left UnclosedParenthesis

parseApp :: ExprParser
parseApp tokens = do
    (firstExpr, remainingTokens) <- parseExpr tokens
    buildApplication firstExpr remainingTokens

buildApplication :: Expression -> [String] -> ParserResult
buildApplication expr [] = Right (expr, [])
buildApplication expr (")":ts) = Right (expr, (")":ts))
buildApplication expr ts = do
    (nextExpr, remainingTokens) <- parseExpr ts
    buildApplication (App expr nextExpr) remainingTokens
