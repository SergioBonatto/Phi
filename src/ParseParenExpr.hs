module ParseParenExpr (ParenExprParser(..)) where

import ParseTypes (Parser(..), ParseError(..))
import ParseApp (AppParser(..))

data ParenExprParser = ParenExprParser

instance Parser ParenExprParser where
    parse _ toks = do
        (appExpr, remainingTokens) <- parse AppParser toks
        case remainingTokens of
            [] -> Left UnclosedParenthesis
            (")":rest) -> Right (appExpr, rest)
            _ -> Left UnclosedParenthesis
