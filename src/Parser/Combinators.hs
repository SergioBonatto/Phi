module Combinators (
    sequenceParsers,
    choiceParsers,
    tokenParser
) where

import Expression (Expression)
import Types (Result, ExprParser, ParserError(..))
import qualified Data.Map as Map

-- | Combina dois parsers em sequência
sequenceParsers :: (Expression -> Expression -> Expression)
                -> Result
                -> Result
                -> Result
sequenceParsers combine p1 p2 = case p1 of
    Left err -> Left err
    Right (expr1, rest1) -> case p2 of
        Left err -> Left err
        Right (expr2, rest2) -> Right (combine expr1 expr2, rest2)

-- | Tenta vários parsers em sequência até encontrar um que funcione
choiceParsers :: [Result] -> Result
choiceParsers [] = Left (UnexpectedEndOfInput "Sem mais opções de parse")
choiceParsers (p:ps) = case p of
    Right result -> Right result
    Left _ -> choiceParsers ps

-- | Parser que verifica se o próximo token é o esperado
tokenParser :: String -> ExprParser
tokenParser expected (tok:rest)
    | tok == expected = Right (undefined, rest)
    | otherwise = Left (UnexpectedToken expected tok)
tokenParser expected [] =
    Left (UnexpectedEndOfInput $ "Esperava '" ++ expected ++ "'")
