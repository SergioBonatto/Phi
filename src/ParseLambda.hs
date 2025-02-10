module ParseLambda (LambdaParser(..)) where

import Expression (Expression(..))
import ParseTypes (Parser(..), ParseError(..))
import ParseVar (VarParser(..)) 

data LambdaParser = LambdaParser

instance Parser LambdaParser where
    parse _ [] = Left UnexpectedEndOfInput
    parse _ (var:".":rest) = do
        (body, remaining) <- parse VarParser rest
        Right (Lam var body, remaining) -- Corrigido: removido 'Lam' extra e usando Right
    parse _ _ = Left (UnexpectedToken "Expected variable and dot")
