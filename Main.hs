module Main where

import System.IO (getContents)
import Lexer (runAlex)
import Parser (pFooz)

-- Parsing function
parse :: String -> Either String Foo
parse s = runAlex s pFooz

main :: IO ()
main = do
  -- Read from standard input
  input <- getContents
  case parse input of
    Right result -> putStrLn $ "Parsed successfully: " ++ show result
    Left err     -> putStrLn $ "Parse error: " ++ err
