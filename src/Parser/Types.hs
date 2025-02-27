{-# LANGUAGE MultiParamTypeClasses #-}

module Types (
    InterpreterConfig(..),
    Extension(..),
    LogLevel(..),

    Parser(..),
    ParserError(..),
    Result,
    ExprParser,

    EvalTrace(..),
    MemoTable,
    EvalResult,

    LogEntry(..)
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Expression (Expression)
import Data.Time (UTCTime)
import Error (ParserError(..))

data Extension =
    TypeSystem
  | PatternMatching
  | RecordSyntax
  deriving (Show, Eq, Ord)

data InterpreterConfig = InterpreterConfig {
    maxSteps :: Int,
    debug :: Bool,
    tracing :: Bool,
    extensions :: Set.Set Extension,
    memoization :: Bool
} deriving (Show, Eq)

data LogLevel = Debug | Info | Warning | Error
    deriving (Show, Eq, Ord)

data LogEntry = LogEntry {
    timestamp :: UTCTime,
    level :: LogLevel,
    message :: String,
    context :: Map.Map String String
} deriving (Show)

data EvalTrace = EvalTrace {
    step :: Int,            -- Número do passo
    expr :: Expression,     -- Expressão avaliada
    redex :: Maybe String,  -- Descrição opcional do redex
    memoryUsage :: Int,     -- Uso de memória
    optimization :: Maybe String  -- Otimização aplicada (se houver)
} deriving (Show)

type MemoKey = (Expression, Set.Set String, Map.Map String Expression)
type MemoValue = (Expression, Int, [EvalTrace])
type MemoTable = Map.Map MemoKey MemoValue

type EvalResult = (Expression, Int, [EvalTrace], MemoTable)

class Monad m => Parser p m where
    parse :: p -> [String] -> m Result
    parseWithContext :: p -> [String] -> Map.Map String Expression -> m Result

type Result = Either ParserError (Expression, [String])

type ExprParser = [String] -> Result
