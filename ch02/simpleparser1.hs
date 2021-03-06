module Main where 
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main:: IO ()
main = do
  (arg:args) <- getArgs
  putStrLn( readExpr arg )

symbol:: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

readExpr:: String -> String
readExpr input = case parse symbol "lisp" input of 
 Left err -> "No match: " ++ (show err) 
 Right val -> "Match found: " ++ show val

