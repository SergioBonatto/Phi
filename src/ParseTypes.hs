module ParseTypes (
    ExprParser,
    ParserResult
) where

import Expression (Expression)
import ParseError (ParseError)

type ParserResult = Either ParseError (Expression, [String])
type ExprParser = [String] -> ParserResult
