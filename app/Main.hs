{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Applicative            hiding (many)
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Except
import           Data.Functor
import qualified Data.Map                       as Map
import           Data.Map                       ((!?), Map)
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
parseStart = parseNegation <|> parseVar <|> parseInt

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

data EvalError = BadAssignment | OutOfScope
  deriving Show

type GlobalState = Map Text Int
type EvalM = StateT GlobalState (Except EvalError) Int

runEvalM :: EvalM -> GlobalState -> Either EvalError (Int, GlobalState)
runEvalM eval = runExcept . runStateT eval

eval :: Expr -> EvalM
eval = \case
  Var s -> do
    mint <- gets (flip (!?) s)
    case mint of
      Nothing -> throwError OutOfScope
      Just i -> pure i
  Int x -> pure x
  Assignment (Var s) expr -> do
    val <- eval expr
    modify (Map.insert s val)
    pure val
  Assignment expr _ -> throwError BadAssignment
  Negation expr -> eval expr >>= pure . negate
  Sum expr' expr'' -> do
    x <- eval expr'
    y <- eval expr''
    pure (x + y)
  Product expr' expr'' -> do
    x <- eval expr'
    y <- eval expr''
    pure (x * y)
  Division expr' expr'' -> do
    x <- eval expr'
    y <- eval expr''
    pure (x * y)

evalList :: [Expr] -> EvalM
evalList = fmap head . traverse eval


----------
--- IO ---
----------

repl :: GlobalState -> IO ()
repl initialState = do
  input <- runParser parseExpr mempty . T.pack <$> getLine
  case input of
    Left e -> print e
    Right ast ->
      case runEvalM (eval ast) initialState of
        Left e -> print e >> repl initialState
        Right (result, state) -> print result >> repl state

runRepl :: IO ()
runRepl = repl Map.empty

main :: IO ()
main = do
  file <- T.pack <$> readFile "input.txt"

  let exprs = runParser parseExprs mempty file
  case exprs of
    Left e -> print e
    Right a ->
      case runEvalM (evalList a) Map.empty of
        Left e -> print e
        Right (result, _) -> print result
