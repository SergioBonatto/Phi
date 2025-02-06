module ParseCommon (parseExpr) where

import Expression (Expression(..))

parseExpr :: [String] -> (Expression, [String])
parseExpr [] = error "Unexpected end of expression"
parseExpr (tok:toks)
    | tok == "λ" = parseLambda toks
    | tok == "(" = parseParenExpr toks
    | tok == ")" || tok == "." || tok == "=" = error ("Unexpected token: " ++ tok)
    | otherwise = (Var tok, toks)

parseLambda :: [String] -> (Expression, [String])
parseLambda (var:".":rest) =
    let (body, remainingTokens) = parseExpr rest
    in (Lam var body, remainingTokens)
parseLambda _ = error "Invalid syntax for lambda expression"

parseParenExpr :: [String] -> (Expression, [String])
parseParenExpr toks =
    let (appExpr, remainingTokens) = parseApp toks
    in case remainingTokens of
         [] -> error "Unclosed parenthesis"
         (")":rest) -> (appExpr, rest)
         _ -> error "Unclosed parenthesis"

parseApp :: [String] -> (Expression, [String])
parseApp tokens =
    let (firstExpr, remainingTokens) = parseExpr tokens
    in parseApp' firstExpr remainingTokens

parseApp' :: Expression -> [String] -> (Expression, [String])
parseApp' expr [] = (expr, [])
parseApp' expr (")":ts) = (expr, (")":ts))
parseApp' expr ts =
    let (nextExpr, remainingTokens) = parseExpr ts
    in parseApp' (App expr nextExpr) remainingTokens
