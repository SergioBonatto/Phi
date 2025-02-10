module Var (VarParser(..)) where

import Expression (Expression(..))
import Types (Parser(..), Error(..))

data VarParser = VarParser

instance Parser VarParser where
    parse _ [] = Left UnexpectedEndOfInput
    parse _ (tok:toks) = Right (Var tok, toks)
