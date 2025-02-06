{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Evaluator (evaluate)
import Parser (processCode)
import Text.Printf (printf)

main :: IO ()
main = do
    args <- getArgs
    case args of
      [] -> do
           putStrLn "Usage: phi <file> [-s] [-c]"
           exitFailure
      (filePath:opts) -> do
           let showStats   = "-s" `elem` opts
               showContext = "-c" `elem` opts
           code <- readFile filePath
           startTime <- getCurrentTime
           let (lastExpr, env) = processCode code
               usedDefs = Set.empty  -- For simplicity, we do not track used definitions
               (result, steps) = evaluate lastExpr env usedDefs 1000
           endTime <- getCurrentTime
           let elapsedTime = realToFrac (diffUTCTime endTime startTime) :: Double
           putStrLn "Final result:"
           putStrLn $ "=> " ++ show result
           if showStats then do
             putStrLn "=================================================="
             printf "Execution time: %.6f seconds\n" elapsedTime
             putStrLn $ "Number of reduction steps: " ++ show steps
           else return ()
           if showContext then do
             putStrLn "=================================================="
             putStrLn "Used definitions:"
             mapM_ (\(n,e) -> putStrLn $ n ++ " = " ++ show e) (Map.toList env)
           else return ()
