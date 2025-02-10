module ParenExpr (ParenExprParser(..)) where

import Types (Parser(..), Error(..))
import App (AppParser(..))

data ParenExprParser = ParenExprParser

instance Parser ParenExprParser where
    parse _ toks = do
        (appExpr, remainingTokens) <- parse AppParser toks
        case remainingTokens of
            [] -> Left UnclosedParenthesis
            (")":rest) -> Right (appExpr, rest)
            _ -> Left UnclosedParenthesis
