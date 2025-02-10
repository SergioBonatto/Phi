module ParseCombinators (
    parseSequence,
    parseChoice,
    parseToken
) where

import Expression (Expression)
import ParseTypes (ParserResult, ExprParser)
import ParseError (ParseError(..))

parseSequence :: (Expression -> Expression -> Expression) -> ParserResult -> ParserResult -> ParserResult
parseSequence combine p1 p2 = case p1 of
    Left err -> Left err
    Right (expr1, rest1) -> case p2 of
        Left err -> Left err
        Right (expr2, rest2) -> Right (combine expr1 expr2, rest2)

parseChoice :: [ParserResult] -> ParserResult
parseChoice [] = Left UnexpectedEndOfInput
parseChoice (p:ps) = case p of
    Right result -> Right result
    Left _ -> parseChoice ps

parseToken :: String -> ExprParser
parseToken expected (tok:rest) | tok == expected = Right (undefined, rest)
parseToken expected _ = Left (UnexpectedToken expected)
