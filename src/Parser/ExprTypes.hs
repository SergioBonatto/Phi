module ExprTypes (ParserFunction) where

import Types (Result)

type ParserFunction = [String] -> Result
