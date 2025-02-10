module ParseVar (VarParser(..)) where

import Expression (Expression(..))
import ParseTypes (Parser(..), ParseError(..))

data VarParser = VarParser

instance Parser VarParser where
    parse _ [] = Left UnexpectedEndOfInput
    parse _ (tok:toks) = Right (Var tok, toks)
