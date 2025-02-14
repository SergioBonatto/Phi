{-# LANGUAGE MultiParamTypeClasses #-}

module Types (
    -- * Tipos principais
    InterpreterConfig(..),
    Extension(..),
    LogLevel(..),

    -- * Parser
    Parser(..),
    ParserError(..),
    Result,
    ExprParser,

    -- * Avaliação
    EvalTrace(..),
    MemoTable,
    EvalResult,

    -- * Logging
    LogEntry(..)
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Expression (Expression)
import Data.Time (UTCTime)
import Error (ParserError(..))

-- | Extensões disponíveis do interpretador
data Extension =
    TypeSystem        -- ^ Sistema de tipos
  | PatternMatching  -- ^ Pattern matching
  | RecordSyntax     -- ^ Sintaxe de registros
  deriving (Show, Eq, Ord)

-- | Configuração do interpretador
data InterpreterConfig = InterpreterConfig {
    maxSteps :: Int,                -- ^ Limite máximo de passos
    debug :: Bool,                  -- ^ Modo debug
    tracing :: Bool,                -- ^ Rastreamento de avaliação
    extensions :: Set.Set Extension, -- ^ Extensões ativas
    memoization :: Bool             -- ^ Cache de expressões
} deriving (Show, Eq)

-- | Níveis de log
data LogLevel = Debug | Info | Warning | Error
    deriving (Show, Eq, Ord)

-- | Entrada de log
data LogEntry = LogEntry {
    timestamp :: UTCTime,
    level :: LogLevel,
    message :: String,
    context :: Map.Map String String
} deriving (Show)

-- | Trace de avaliação
data EvalTrace = EvalTrace {
    step :: Int,                   -- ^ Número do passo
    expr :: Expression,            -- ^ Expressão atual
    redex :: Maybe String,         -- ^ Redex encontrado
    memoryUsage :: Int,            -- ^ Uso de memória
    optimization :: Maybe String    -- ^ Otimização aplicada
} deriving (Show)

-- | Cache de avaliações
type MemoKey = (Expression, Set.Set String, Map.Map String Expression)
type MemoValue = (Expression, Int, [EvalTrace])
type MemoTable = Map.Map MemoKey MemoValue

-- | Resultado de avaliação incluindo trace e cache
type EvalResult = (Expression, Int, [EvalTrace], MemoTable)

-- | Parser monádico com suporte a contexto
class Monad m => Parser p m where
    parse :: p -> [String] -> m Result
    parseWithContext :: p -> [String] -> Map.Map String Expression -> m Result

type Result = Either ParserError (Expression, [String])

-- | Parser de expressões não-monádico
type ExprParser = [String] -> Result
