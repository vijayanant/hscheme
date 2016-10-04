module Main where 
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Error

main:: IO ()
main = do 
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool


eval :: LispVal -> ThrowsError LispVal
eval val@(Atom _)                = return val
eval val@(Number _)              = return val
eval val@(String _)              = return val
eval val@(Bool _)                = return val
eval (List [Atom "quote", val ]) = return val
eval (List (Atom func : args))   = mapM eval args >>= apply func
eval badform                     = throwError $ BadSpecialForm "Unrecognized special form" badform


apply:: String  -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function" func ) 
                        ($ args)  
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
  ("+", numbericBinOp (+)),
  ("-", numbericBinOp (-)),
  ("*", numbericBinOp (*)),
  ("/", numbericBinOp div),
  ("mod", numbericBinOp mod),
  ("quotient", numbericBinOp quot),
  ("remainder", numbericBinOp rem)
  ]

numbericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
{-numbericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal-}
numbericBinOp op args   = mapM unpackNumber args >>= return . Number . foldl1 op

unpackNumber :: LispVal -> ThrowsError Integer
unpackNumber (Number n) = return n
unpackNumber (String n) = let parsed = reads n
                              in if null parsed 
                                  then throwError $ TypeMismatch "number" $ String n
                                  else return $ fst $ parsed !! 0
unpackNumber (List [n]) = unpackNumber n
unpackNumber x = throwError $ TypeMismatch "number" x

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

data LispError  = NumArgs Integer [ LispVal ]
                | TypeMismatch String LispVal
                | Parser ParseError
                | BadSpecialForm String LispVal 
                | NotFunction String String
                | UnboundVar String String
                | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found )     = "Expected " ++ show expected ++ " args: found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where
  noMsg  = Default "An error has occurred"
  strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val ) = val

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

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err 
  Right val -> return val

