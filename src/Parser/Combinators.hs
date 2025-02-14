module Combinators (
    sequenceParsers,
    choiceParsers,
    tokenParser
) where

import Expression (Expression)
import Types (Result, ExprParser, ParserError(..))
import qualified Data.Map as Map

sequenceParsers :: (Expression -> Expression -> Expression)
                -> Result
                -> Result
                -> Result
sequenceParsers combine p1 p2 = case p1 of
    Left err -> Left err
    Right (expr1, rest1) -> case p2 of
        Left err -> Left err
        Right (expr2, rest2) -> Right (combine expr1 expr2, rest2)

choiceParsers :: [Result] -> Result
choiceParsers [] = Left (UnexpectedEndOfInput "Sem mais opções de parse")
choiceParsers (p:ps) = case p of
    Right result -> Right result
    Left _ -> choiceParsers ps

tokenParser :: String -> ExprParser
tokenParser expected (tok:rest)
    | tok == expected = Right (undefined, rest)
    | otherwise = Left (UnexpectedToken expected tok)
tokenParser expected [] =
    Left (UnexpectedEndOfInput $ "Esperava '" ++ expected ++ "'")
