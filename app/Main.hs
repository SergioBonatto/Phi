module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad (when)
import Evaluator (evaluate)
import Parser (processCode)
import Text.Printf (printf)
import Types (InterpreterConfig(..), EvalTrace(..))
import Data.Maybe (fromMaybe)


main :: IO ()
main = do
    args <- getArgs
    case args of
      [] -> do
           putStrLn "Usage: phi <file> [-s] [-c] [-d] [-t] [-m steps]"
           exitFailure
      (filePath:opts) -> do
           let showStats    = "-s" `elem` opts
               showContext  = "-c" `elem` opts
               showDebug    = "-d" `elem` opts
               showTrace    = "-t" `elem` opts
               stepLimit    = maybe 1000 read $ lookup "-m" $ zip opts (drop 1 opts)

           let config = InterpreterConfig {
               maxSteps    = stepLimit,
               debug       = showDebug,
               tracing     = showTrace,
               extensions  = Set.empty,
               memoization = False
           }

           code <- readFile filePath
           startTime <- getCurrentTime

           case processCode code of
               Left err -> do
                   putStrLn "Error parsing code:"
                   print err
                   exitFailure
               Right (lastExpr, env) ->
                   case evaluate config lastExpr env Set.empty of
                       (result, steps, traces) -> do
                           endTime <- getCurrentTime
                           let elapsedTime = realToFrac (diffUTCTime endTime startTime) :: Double

                           putStrLn "Final result:"
                           putStrLn $ "=> " ++ show result

                           when showStats $ do
                               putStrLn "=================================================="
                               printf "Execution time: %.6f seconds\n" elapsedTime
                               printf "Number of reduction steps: %d\n" steps

                           when showTrace $ do
                               putStrLn "=================================================="
                               putStrLn "Evaluation trace:"
                               mapM_ (\t -> printf "[%d] %s: %s\n" (step t) (fromMaybe "" (redex t)) (show (expr t))) traces
                           when showContext $ do
                               putStrLn "=================================================="
                               putStrLn "Environment:"
                               mapM_ (\(n,e) -> putStrLn $ n ++ " = " ++ show e) (Map.toList env)
