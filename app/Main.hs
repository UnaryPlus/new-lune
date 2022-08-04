module Main (main) where

import qualified Data.Text.IO as IO

import Parse (parseFile)
import Infer (runInfer)
import Syntax.Core

-- M.ParseErrorBundle Text Void

main :: IO ()
main = do
  file <- IO.readFile "test.lune"
  case parseFile file of
    Left err -> print err
    Right e -> case runInfer e of
      Left err -> IO.putStrLn (pretty err)
      Right t -> IO.putStrLn (pretty t)
