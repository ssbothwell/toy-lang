{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative            hiding (many)
import           Control.Monad
import           Control.Monad.State
import           Data.Functor
import qualified Data.Map                       as Map
import           Data.Map                       ((!), Map)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Text.Encoding             (decodeUtf8)
import           Data.Void
import           System.IO
import           Text.Megaparsec                hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L


---------------
--- Parsing ---
---------------

{-
New Grammar:

EXPR = "(" EXPR ")" | START END
START = "-" EXPR | VAR | DIGIT
END = OP EXPR | NOTHING
OP = "-" | "+" | "*" | "/" | "="

Old Grammar:

EXPR = VAR | DIGIT | SUM | SUB | NEGATE | PRODUCT | DIV | ASSIGN

SUM = EXPR "+" EXPR;
SUB = EXPR "-" EXPR;
NEGATE = "-" EXPR;
PRODUCT = EXPR "*" EXPR;
DIV = EXPR "/" EXPR;
ASSIGN = VAR "=" EXPR;

VAR = ALPHA { ALPHA | DIGIT };
DIGIT = "0".."9";

ALPHA = "A".."Z" | "a".."z";
INTEGER = DIGIT {DIGIT};
-}

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

parseNegation :: Parser Expr
parseNegation = parseMinus *> parseExpr >>= pure . Negation

parseExprs :: Parser [Expr]
parseExprs = parseExpr `sepBy` parseSemi

parseExpr :: Parser Expr
parseExpr = do
  t1 <- try (parseParens parseExpr) <|> parseStart
  mT2 <- parseEnd
  case mT2 of
    Epsilon      -> pure t1
    AddTag t2    -> pure (Sum t1 t2)
    SubTag t2    -> pure (Sum t1 (Negation t2))
    MulTag t2    -> pure (Product t1 t2)
    DivTag t2    -> pure (Division t1 t2)
    AssignTag t2 -> pure (Assignment t1 t2)

parseStart :: Parser Expr
parseStart = parseNegation <|> parseVar <|> parseInt <|> undefined

data Tag a = AddTag a | SubTag a | MulTag a | DivTag a | AssignTag a | Epsilon

parseEnd  :: Parser (Tag Expr)
parseEnd = choice [pAdd, pSub, pMul, pDiv, pAssign, pure Epsilon]
  where
    pAdd    = parsePlus  *> (AddTag    <$> parseExpr)
    pSub    = parseMinus *> (SubTag    <$> parseExpr)
    pMul    = parseStar  *> (MulTag    <$> parseExpr)
    pDiv    = parseSlash *> (DivTag    <$> parseExpr)
    pAssign = parseEqual *> (AssignTag <$> parseExpr)


------------------
--- Evaluation ---
------------------

data Expr
  = Var Text
  | Int Int
  | Negation Expr
  | Sum Expr Expr
  | Product Expr Expr
  | Division Expr Expr
  | Assignment Expr Expr
  deriving (Show, Eq)

type GlobalState = Map Text Int

eval :: Expr -> State GlobalState Int
eval (Var s) = gets (flip (!) s)
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

evalList :: [Expr] -> State GlobalState Int
evalList exprs = let x = traverse eval exprs in head <$> x


----------
--- IO ---
----------

repl :: GlobalState -> IO ()
repl initialState = do
  input <- runParser parseExpr mempty . T.pack <$> getLine
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

  let exprs = runParser parseExprs mempty file
  case exprs of
    Left e -> print e
    Right a ->
      let result = evalState (evalList a) Map.empty
      in print result
