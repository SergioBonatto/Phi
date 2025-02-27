{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Data.Text (Text)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data Expression
    = Var Text
    | Lam Text Expression
    | App Expression Expression
    deriving (Show, Eq, Ord)

type Env = Map.Map Text Expression

newtype MemoTable = MemoTable (Map.Map Expression Expression)
    deriving (Semigroup, Monoid)

data InterpreterConfig = InterpreterConfig
    { maxSteps :: Int
    , debug :: Bool
    , tracing :: Bool
    , extensions :: Set.Set Extension
    , memoization :: Bool
    }

data Extension
    = TypeSystem
    | PatternMatching
    | RecordSyntax
    deriving (Show, Eq, Ord)
