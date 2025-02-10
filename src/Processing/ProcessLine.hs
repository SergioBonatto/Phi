module ProcessLine (processLine) where

import Expression (Expression)
import Environment (Env)
import Error (Error)
import Tokenize (tokenize)
import ProcessSingleLine (processSingleLine)

processLine :: Either Error (Expression, Env, Maybe Expression) -> String -> Either Error (Expression, Env, Maybe Expression)
processLine acc line = do
    (_, env, lastExpr) <- acc
    let tokens = tokenize line
    processSingleLine tokens env lastExpr
