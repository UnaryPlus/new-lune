module Main (main) where

import Parse (parseFile)
import Infer (runInfer)

import Syntax

main :: IO ()
main = do
  file <- readFile "test.lune"
  case parseFile file of
    Left err -> print err
    Right e -> case runInfer e of
      Left err -> putStrLn (pretty err)
      Right t -> putStrLn (pretty t)
