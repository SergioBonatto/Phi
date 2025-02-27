module Environment (Env) where

import Data.Map (Map)
import Expression (Expression)

type Env = Map String Expression
