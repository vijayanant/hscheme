module Main where 
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

main:: IO ()
main = do
  (arg:args) <- getArgs
  putStrLn( readExpr arg )

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
              deriving (Show)

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many ( noneOf "\"" )
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom =  do
  first <- letter <|> symbol
  rest <- many (letter <|> symbol <|> digit)
  let atom = [first] ++ rest
  return $ case atom of 
    "#t" -> Bool True
    "#f" -> Bool False
    otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber =  liftM (Number . read) $ many1 digit

parseExpr:: Parser LispVal
parseExpr = parseString 
        <|> parseAtom 
        <|> parseNumber


symbol:: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces:: Parser ()
spaces = skipMany1 space

readExpr:: String -> String
readExpr input = case parse ( parseExpr ) "lisp" input of 
 Left err -> "No match: " ++ (show err) 
 Right val -> "Match found: " ++ show val

