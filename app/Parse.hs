{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Parse (parseFile) where

import Text.Parsec (Parsec, chainr1, string, spaces, between, chainl1, satisfy, choice, notFollowedBy, many, ParseError, runParser, eof)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (isAsciiUpper, isAsciiLower, isDigit)

import Syntax

data Nat = Z | S Nat
  deriving (Eq)

data CommentState
  = Code
  | Line
  | Block Nat
  deriving (Eq)

removeComments :: String -> String
removeComments = recurse Code
  where
    recurse state = \case
      '-':'-':s -> recurse (lineStart state) s
      '\n':s -> '\n' : recurse (lineEnd state) s
      '[':'-':s -> recurse (blockStart state) s
      '-':']':s -> ' ' : recurse (blockEnd state) s
      c:s | state == Code -> c : recurse state s
          | otherwise -> recurse state s
      [] -> []

    lineStart = \case
      Code -> Line
      state -> state

    lineEnd = \case
      Line -> Code
      state -> state

    blockStart = \case
      Code -> Block Z
      Block n -> Block (S n)
      state -> state

    blockEnd = \case
      Block Z -> Code
      Block (S n) -> Block n
      state -> state

type Parser = Parsec String ()

infixl 5 |.
(|.) :: Applicative f => f a -> f b -> f a
(|.) = (<*)

constant :: a -> String -> Parser a
constant x s = string s >> spaces >> return x

symbol :: String -> Parser ()
symbol = constant ()

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

squares :: Parser a -> Parser a
squares = between (symbol "[") (symbol "]")

nameStart :: Parser Char
nameStart = satisfy \c -> isAsciiUpper c || isAsciiLower c

nameChar :: Parser Char
nameChar = satisfy \c -> isAsciiUpper c || isAsciiLower c || isDigit c

parseName :: [String] -> Parser String
parseName reserved = do
  notFollowedBy (choice (map string reserved) >> notFollowedBy nameChar)
  (:) <$> nameStart <*> many nameChar |. spaces

keywords :: [String]
keywords = [ "val", "type", "in" ]

typeConstants :: Map String (Parser TConst)
typeConstants = Map.fromList
  [ ("forall", Forall <$> squares parseKind)
  , ("arrow", return Arrow)
  , ("nil", return Nil)
  , ("cons", return Cons)
  , ("record", return Record)
  , ("variant", return Variant)
  , ("infrecord", return InfRecord)
  , ("infvariant", return InfVariant)
  ]

termConstants :: Map String (Parser Const)
termConstants = Map.fromList
  [ ("unit", return Unit)
  , ("project", return Project)
  , ("delete", return Delete)
  , ("construct", return Construct)
  , ("absurd ", return Absurd)
  , ("inject", return Inject)
  , ("embed", return Embed)
  , ("destruct", return Destruct)
  ]

nameParser :: Map String (Parser c) -> (String -> a) -> (c -> a) -> Parser a
nameParser constants var con = do
  name <- parseName keywords
  case Map.lookup name constants of
    Nothing -> return (var name)
    Just p -> con <$> p

parseTVar :: Parser TVar
parseTVar = TV <$> parseName (keywords ++ Map.keys typeConstants)

parseNameType :: Parser Type
parseNameType = nameParser typeConstants (TVar . TV) TConst

parseVar :: Parser Var
parseVar = V <$> parseName (keywords ++ Map.keys termConstants)

parseNameTerm :: Parser Term
parseNameTerm = nameParser termConstants (Var . V) Const

parseKind :: Parser Kind
parseKind = chainr1 factor (constant (~>) "->")
  where
    factor = choice
      [ constant KType "Type"
      , constant KRow "Row"
      , constant KLabel "Label"
      , parens parseKind
      ]

parseType :: Parser Type
parseType = chainr1 factor (constant (~>) "->")
  where
    factor = chainl1 appFactor (return TApp)

    appFactor = choice
      [ parseNameType
      , pure TLam |. symbol "\\" <*> parseTVar |. symbol ":" <*> parseKind |. symbol "." <*> parseType
      , pure (TConst . Label . Lab) |. symbol "'" <*> parseName []
      , parens parseType
      ]

parseTerm :: Parser Term
parseTerm = do
  func <- appFactor
  args <- many argument
  return $ foldl (\f -> either (AppT f) (App f)) func args
  where
    argument = choice
      [ Left <$> squares parseType
      , Right <$> appFactor
      ]

    appFactor = choice
      [ parseNameTerm
      , pure Lam |. symbol "\\" <*> parseVar |. symbol ":" <*> parseType |. symbol "." <*> parseTerm
      , pure LamT |. symbol "/\\" <*> parseTVar |. symbol ":" <*> parseKind |. symbol "." <*> parseTerm
      , parens parseTerm
      ]

parseFile :: String -> Either ParseError Term
parseFile = runParser (spaces >> parseTerm |. eof) () "test.lune" . removeComments
