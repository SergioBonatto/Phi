module ProcessLine (processLine) where

import Expression (Expression)
import Environment (Env)
import ParseError (ParseError)
import Tokenize (tokenize)
import ProcessSingleLine (processSingleLine)

processLine :: Either ParseError (Expression, Env, Maybe Expression) -> String -> Either ParseError (Expression, Env, Maybe Expression)
processLine acc line = do
    (_, env, lastExpr) <- acc
    let tokens = tokenize line
    processSingleLine tokens env lastExpr
