module Logger (
    LogLevel(..),
    LogEntry(..),
    MonadLogger(..)
) where

import Data.Time (UTCTime)
import qualified Data.Map as Map

data LogLevel = Debug | Info | Warning | Error
    deriving (Show, Eq, Ord)

data LogEntry = LogEntry {
    timestamp :: UTCTime,
    level :: LogLevel,
    message :: String,
    context :: Map.Map String String
} deriving (Show)

class Monad m => MonadLogger m where
    logMessage :: LogEntry -> m ()
