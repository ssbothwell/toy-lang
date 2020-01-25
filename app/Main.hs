{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative            hiding (many)
import           Control.Monad
import           Control.Monad.State
import           Data.Functor
import qualified Data.Map                       as Map
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Text.Encoding             (decodeUtf8)
import           Data.Void
import           System.IO
import           Text.Megaparsec                hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

rws :: [Text]
rws = ["=", "+", "-", "*", "/" ]

identifier :: Parser Text
identifier = (lexeme . try) (p >>= check)
  where
    p :: Parser Text
    p = T.cons <$> Text.Megaparsec.Char.letterChar <*> (T.pack <$> many Text.Megaparsec.Char.alphaNumChar)
    check :: Text -> Parser Text
    check str = if str `elem` rws
                 then fail $ "keyword " ++ show str ++ " cannot be an identifier"
                 else pure str

parseParens :: Parser a -> Parser a
parseParens = between (symbol "(") (symbol ")")

parseSemi :: Parser ()
parseSemi = void $ symbol ";"

parseEqual :: Parser ()
parseEqual = void $ symbol "="

parsePlus :: Parser ()
parsePlus = void $ symbol "+"

parseMinus :: Parser ()
parseMinus = void $ symbol "-"

parseSlash :: Parser ()
parseSlash = void $ symbol "/"

parseStar :: Parser ()
parseStar = void $ symbol "*"

parseInt :: Parser Expr
parseInt = lexeme $ Int <$> L.decimal

parseVar :: Parser Expr
parseVar = lexeme $ Var <$> identifier

parseOp :: Parser () -> (Expr -> Expr -> Expr) -> Parser Expr
parseOp pOp cnstr = lexeme $ do
  x <- parseTerm
  pOp
  y <- parseTerm
  pure $ x `cnstr` y

parseAdd :: Parser Expr
parseAdd = parseOp parsePlus Sum

parseMultiply :: Parser Expr
parseMultiply = parseOp parseStar Product

parseDivide :: Parser Expr
parseDivide = parseOp parseSlash Division

parseNegation :: Parser Expr
parseNegation = lexeme $ do
  parseMinus
  term <- parseTerm
  pure $ Negation term

parseEquals :: Parser Expr
parseEquals = lexeme $ do
  var <- parseVar
  parseEqual
  term <- parseTerm
  pure $ Assignment var term

parseTerm :: Parser Expr
parseTerm = parseEquals <|> parseAdd <|> parseMultiply <|> parseDivide <|> parseInt <|> parseVar

data Expr
  = Var Text
  | Int Int
  | Negation Expr
  | Sum Expr Expr
  | Product Expr Expr
  | Division Expr Expr
  | Assignment Expr Expr
  deriving (Show, Eq)

lookupVar :: Map.Map Text Int -> Text -> Int
lookupVar map s = case (Map.lookup s map)  of
              Nothing -> error $ "Could not find variable named " <> show s <> "\n" <> show map
              Just x  -> x

type GlobalState = Map.Map Text Int

eval :: Expr -> State GlobalState Int
eval (Var s) = gets (flip lookupVar s)
eval (Int x) = pure x
eval (Assignment (Var s) expr) = do
  val <- eval expr
  modify (Map.insert s val)
  pure val
eval (Assignment expr _) = error $ "Expected left side of assignment to be a variable. Got " <> show expr
eval (Negation expr) = eval expr >>= pure . negate
eval (Sum expr' expr'') = do
  x <- eval expr'
  y <- eval expr''
  pure (x + y)
eval (Product expr' expr'') = do
  x <- eval expr'
  y <- eval expr''
  pure (x * y)
eval (Division expr' expr'') = do
  x <- eval expr'
  y <- eval expr''
  pure (x * y)

--evalList :: [Expr] -> Map.Map Text Int
--evalList = foldl (\b a -> fst (eval b a)) mempty

repl :: GlobalState -> IO ()
repl initialState = do
  input <- runParser parseTerm mempty . T.pack <$> getLine
  case input of
    Left e -> print e
    Right ast -> do
      let (result, state) = runState (eval ast) initialState
      print result
      repl state

runRepl :: IO ()
runRepl = repl Map.empty

main :: IO ()
main = do
  file <- T.pack <$> readFile "input.txt"

  let ast = runParser parseTerm "input.txt" file

  case ast of
    Left e -> print e
    Right a -> do
      -- putStrLn $ "AST: " <> show a
      print (eval Map.empty a)
