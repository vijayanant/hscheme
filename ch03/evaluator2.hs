module Main where 
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

main:: IO ()
main = getArgs >>= putStrLn . show . eval .  readExpr . (!! 0)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

eval :: LispVal -> LispVal
eval val@(Atom _) = val
eval (List [Atom "quote", val ]) = val
eval val@(Number _) = val
eval val@(String _) = val
eval val@(Bool _) = val

showVal :: LispVal -> String
showVal (Atom name)            = name
showVal (List l)               = "(" ++ unwordsList l ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Number i)             = show i
showVal (String s)             = "\"" ++ s ++ "\""
showVal (Bool True)            = "#t"
showVal (Bool False)           = "#f"

unwordsList :: [ LispVal ] -> String
unwordsList = unwords . map showVal

instance Show LispVal where 
  show = showVal

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

parseList :: Parser LispVal
parseList = do 
  liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do 
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do 
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x ]

parseExpr:: Parser LispVal
parseExpr = parseString 
        <|> parseAtom 
        <|> parseNumber
        <|> parseQuoted
        <|> do
              char '('
              x <- (try parseList) <|> parseDottedList
              char ')'
              return x



symbol:: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces:: Parser ()
spaces = skipMany1 space

readExpr:: String -> LispVal
readExpr input = case parse ( parseExpr ) "lisp" input of 
 Left err -> String $ "No match: " ++ (show err) 
 Right val -> val

