{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative            hiding (many)
import           Control.Monad
import           Control.Monad.Combinators.Expr
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

parseSubtract :: Parser Expr
parseSubtract = parseOp parseMinus Subtr

parseMultiply :: Parser Expr
parseMultiply = parseOp parseStar Product

parseDivide :: Parser Expr
parseDivide = parseOp parseSlash Division

parseEquals :: Parser Expr
parseEquals = lexeme $ do
  var <- parseVar
  parseEquals
  term <- parseTerm
  pure $ Assignment var term

parseTerm :: Parser Expr
parseTerm = parseInt <|> parseVar <|> parseAdd <|> parseSubtract <|> parseMultiply <|> parseDivide <|> parseEquals

data Expr
  = Var Text
  | Int Int
  | Negation Expr
  | Sum Expr Expr
  | Subtr Expr Expr
  | Product Expr Expr
  | Division Expr Expr
  | Assignment Expr Expr
  deriving (Show, Eq)

lookupVar :: Map.Map Text Int -> Text -> Int
lookupVar map s = case (Map.lookup s map)  of
              Nothing -> error $ "Could not find variable named " <> show s <> "\n" <> show map
              Just x  -> x

eval :: Map.Map Text Int -> Expr -> (Map.Map Text Int, Int)
eval map (Var s)                     = (map, lookupVar map s)
eval map (Int x)                     = (map, x)

eval map (Assignment (Var s) expr) = (Map.insert s val map', val)
                                     where
                                       (map', val) = eval map expr
eval map (Assignment expr _) = error $ "Expected left side of assignment to be a variable. Got " <> show expr

eval map (Negation expr) = (map', negate val)
                           where (map', val) = eval map expr

eval map (Sum expr' expr'') = (map'', val + val')
                              where
                                (map', val) = eval map expr'
                                (map'', val') = eval map' expr''

eval map (Subtr expr' expr'') = (map'', val - val')
                              where
                                (map', val) = eval map expr'
                                (map'', val') = eval map' expr''

eval map (Product expr' expr'') = (map'', val * val')
                              where
                                (map', val) = eval map expr'
                                (map'', val') = eval map' expr''

eval map (Division expr' expr'') = (map'', val `div` val')
                              where
                                (map', val) = eval map expr'
                                (map'', val') = eval map' expr''

evalList :: [Expr] -> Map.Map Text Int
evalList = foldl (\b a -> fst (eval b a)) mempty

main :: IO ()
main = do
  file <- T.pack <$> readFile "input.txt"

  let ast = runParser parseTerm "input.txt" file

  case ast of
    Left e -> print e
    Right a -> do
      -- putStrLn $ "AST: " <> show a

      print (evalList a)
