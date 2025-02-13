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
import Types (InterpreterConfig(..), EvalTrace(..))  -- Adicionando EvalTrace(..)

main :: IO ()
main = do
    args <- getArgs
    case args of
      [] -> do
           putStrLn "Usage: phi <file> [-s] [-c] [-d] [-t] [-m steps]"
           exitFailure
      (filePath:opts) -> do
           let showStats   = "-s" `elem` opts
               showContext = "-c" `elem` opts
               debug      = "-d" `elem` opts
               trace      = "-t" `elem` opts
               maxSteps   = maybe 1000 read $ lookup "-m" $ zip opts (tail opts)

           let config = InterpreterConfig {
               maxSteps = maxSteps,
               debug = debug,
               tracing = trace
           }

           code <- readFile filePath
           startTime <- getCurrentTime

           case processCode code of
               Left err -> do
                   putStrLn "Error parsing code:"
                   print err
                   exitFailure
               Right (lastExpr, env) -> do
                   let (result, steps, traces) = evaluate config lastExpr env Set.empty

                   endTime <- getCurrentTime
                   let elapsedTime = realToFrac (diffUTCTime endTime startTime) :: Double

                   putStrLn "Final result:"
                   putStrLn $ "=> " ++ show result

                   when showStats $ do
                       putStrLn "=================================================="
                       printf "Execution time: %.6f seconds\n" elapsedTime
                       putStrLn $ "Number of reduction steps: " ++ show steps

                   when trace $ do
                       putStrLn "=================================================="
                       putStrLn "Evaluation trace:"
                       mapM_ (\t -> printf "Step %d: %s\n" (step t) (show $ expr t)) traces

                   when showContext $ do
                       putStrLn "=================================================="
                       putStrLn "Environment:"
                       mapM_ (\(n,e) -> putStrLn $ n ++ " = " ++ show e) (Map.toList env)
