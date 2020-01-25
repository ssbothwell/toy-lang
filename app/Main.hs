{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative            hiding (many)
import           Control.Monad
import           Control.Monad.Combinators.Expr
import           Data.Functor
import qualified Data.Map                       as Map
import           Data.Text                      (Text)
import qualified Data.Text                      as T
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

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

signedInteger :: Parser Integer
signedInteger = L.signed sc integer

signedFloat :: Parser Double
signedFloat = L.signed sc float

pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme (string keyword <* notFollowedBy alphaNumChar)

data Expr
  = Var String
  | Int Int
  | Negation Expr
  | Sum Expr Expr
  | Subtr Expr Expr
  | Product Expr Expr
  | Division Expr Expr
  | Assignment Expr Expr
  deriving (Show, Eq)

pVariable :: Parser Expr
pVariable = Var <$> lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pInteger :: Parser Expr
pInteger = Int <$> lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

pExprs :: Parser [Expr]
pExprs = many pExpr

semi :: Parser ()
semi = void $ symbol ";"

pTerm :: Parser Expr
pTerm = choice
 [ parens pExpr
 , pVariable
 , pInteger
 ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

binaryR :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binaryR name f = InfixR (f <$ symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "-" Negation
    , prefix "+" id
    ]
  , [ binary "*" Product
    , binary "/" Division
    ]
  , [ binary "+" Sum
    , binary "-" Subtr
    ]
  , [ binaryR "=" Assignment ]
  ]


lookupVar :: Map.Map String Int -> String -> Int
lookupVar map s = case (Map.lookup s map)  of
              Nothing -> error $ "Could not find variable named " <> s <> "\n" <> show map
              Just x  -> x

eval :: Map.Map String Int -> Expr -> (Map.Map String Int, Int)
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

evalList :: [Expr] -> Map.Map String Int
evalList = foldl (\b a -> fst (eval b a)) mempty

main :: IO ()
main = do
  file <- T.pack <$> readFile "input.txt"

  let ast = runParser pExprs "input.txt" file

  case ast of
    Left e -> print e
    Right a -> do
      -- putStrLn $ "AST: " <> show a

      print (evalList a)
