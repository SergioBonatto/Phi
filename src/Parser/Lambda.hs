module Lambda (LambdaParser(..)) where

import Expression (Expression(..))
import Types (Parser(..), Error(..))
import Var (VarParser(..))

data LambdaParser = LambdaParser

instance Parser LambdaParser where
    parse _ [] = Left UnexpectedEndOfInput
    parse _ (var:".":rest) = do
        (body, remaining) <- parse VarParser rest
        Right (Lam var body, remaining)
    parse _ _ = Left (UnexpectedToken "Expected variable and dot")
