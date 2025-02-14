{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Var (VarParser(..)) where

import Expression (Expression(..))
import Types (Parser(..), ParserError(..))

data VarParser = VarParser

instance Monad m => Parser VarParser m where
    parse _ [] = return $ Left (UnexpectedEndOfInput "Expected variable name")
    parse _ (tok:toks) = return $ Right (Var tok, toks)

    parseWithContext p tokens _ = parse p tokens
