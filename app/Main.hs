{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Evaluator (evaluate)
import Parser (processCode)

main :: IO ()
main = do
    args <- getArgs
    case args of
      [] -> do
           putStrLn "Uso: phi <arquivo> [-s] [-c]"
           exitFailure
      (filePath:opts) -> do
           let showStats   = "-s" `elem` opts
               showContext = "-c" `elem` opts
           code <- readFile filePath
           startTime <- getCurrentTime
           let (lastExpr, env) = processCode code
               usedDefs = Set.empty  -- Para simplicidade, não rastreamos definições usadas
               (result, steps) = evaluate lastExpr env usedDefs 1000
           endTime <- getCurrentTime
           let elapsedTime = realToFrac (diffUTCTime endTime startTime) :: Double
           putStrLn "Resultado final:"
           putStrLn $ "=> " ++ show result
           if showStats then do
             putStrLn "=================================================="
             putStrLn $ "Tempo de execução: " ++ show elapsedTime ++ " segundos"
             putStrLn $ "Número de passos de redução: " ++ show steps
           else return ()
           if showContext then do
             putStrLn "=================================================="
             putStrLn "Definições utilizadas:"
             mapM_ (\(n,e) -> putStrLn $ n ++ " = " ++ show e) (Map.toList env)
           else return ()
