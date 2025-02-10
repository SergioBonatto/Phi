module ParseTypes (
    ExprParser,
    ParserResult,
    Parser(..),
    ParseError(..)
) where

import Expression (Expression)
import ParseError (ParseError(..))

type ParserResult = Either ParseError (Expression, [String])
type ExprParser = [String] -> ParserResult

class Parser p where
    parse :: p -> [String] -> ParserResult
