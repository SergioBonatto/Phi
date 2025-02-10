module StripLine (stripLine) where

stripLine :: String -> String
stripLine = unwords . words
