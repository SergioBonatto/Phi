module Tokenize (tokenize) where

import Data.Char (isAlphaNum)

tokenize :: String -> [String]
tokenize [] = []
tokenize ('-':'-':rest) = tokenize $ dropWhile (/= '\n') rest
tokenize ('{':'-':rest) = tokenize $ skipMultiLineComment rest
tokenize (c:cs)
    | c `elem` ("λ().=" :: [Char]) = [c] : tokenize cs
    | c == ' ' || c == '\t' || c == '\n' = tokenize cs
    | otherwise = let (tok, rest) = span isAlphaNum (c:cs)
                  in tok : tokenize rest

skipMultiLineComment :: String -> String
skipMultiLineComment [] = []
skipMultiLineComment ('-':'}':rest) = rest
skipMultiLineComment (_:rest) = skipMultiLineComment rest
