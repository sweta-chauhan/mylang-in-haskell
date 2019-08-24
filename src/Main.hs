module Main
  where
import  Tokenizer
import Parser
import Data.Typeable
import System.Environment
import System.IO


main = do
  args <- getArgs
  content <- readFile (head args)
  let ast = parserMain content
  putStr (show ast) 
