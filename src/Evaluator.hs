module Evaluator (evaluate) where

import Expression (Expression)
import Environment (Env)
import Data.Set (Set)
import EvaluateStep (evaluateStep)

evaluate :: Expression -> Env -> Set String -> Int -> (Expression, Int)
evaluate expr env _usedDefs maxSteps = evaluateStep expr env 0 maxSteps
