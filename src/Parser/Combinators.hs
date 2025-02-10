module Combinators (
    sequenceParsers,
    choiceParsers,
    tokenParser
) where

import Expression (Expression)
import Types (Result, ExprParser)
import Error (Error(..))

sequenceParsers :: (Expression -> Expression -> Expression) -> Result -> Result -> Result
sequenceParsers combine p1 p2 = case p1 of
    Left err -> Left err
    Right (expr1, rest1) -> case p2 of
        Left err -> Left err
        Right (expr2, rest2) -> Right (combine expr1 expr2, rest2)

choiceParsers :: [Result] -> Result
choiceParsers [] = Left UnexpectedEndOfInput
choiceParsers (p:ps) = case p of
    Right result -> Right result
    Left _ -> choiceParsers ps

tokenParser :: String -> ExprParser
tokenParser expected (tok:rest) | tok == expected = Right (undefined, rest)
tokenParser expected _ = Left (UnexpectedToken expected)
