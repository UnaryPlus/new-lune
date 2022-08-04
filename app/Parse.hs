{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse (parseFile) where

import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Monad.Combinators.Expr

import Data.Void (Void)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (isAsciiUpper, isAsciiLower, isDigit)

import Syntax.Core

type Parser = M.Parsec Void Text

spaces :: Parser ()
spaces = L.space C.space1
  (L.skipLineComment "--")
  (L.skipBlockCommentNested "[-" "-]")

symbol :: Text -> Parser ()
symbol s = M.chunk s >> spaces

parens :: Parser a -> Parser a
parens = M.between (symbol "(") (symbol ")")

squares :: Parser a -> Parser a
squares = M.between (symbol "[") (symbol "]")

nameStart :: Char -> Bool
nameStart c = isAsciiUpper c || isAsciiLower c

nameChar :: Char -> Bool
nameChar c = isAsciiUpper c || isAsciiLower c || isDigit c

parseName :: [Text] -> Parser Text
parseName reserved = do
  M.notFollowedBy (M.choice (map reservedWord reserved))
  c <- M.satisfy nameStart
  s <- M.takeWhileP Nothing nameChar
  spaces
  return (Text.cons c s)
  where
    reservedWord :: Text -> Parser ()
    reservedWord r = M.chunk r >> M.notFollowedBy (M.satisfy nameChar)

keywords :: [Text]
keywords = [ "val", "type", "in" ]

typeConstants :: Map Text (Parser TConst)
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

termConstants :: Map Text (Parser Const)
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

nameParser :: Map Text (Parser c) -> (Text -> a) -> (c -> a) -> Parser a
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
parseKind = makeExprParser factor
  [ [ InfixR ((~>) <$ symbol "->") ] ]
  where
    factor = M.choice
      [ KType <$ symbol "Type"
      , KRow <$ symbol "Row"
      , KLabel <$ symbol "Label"
      , parens parseKind
      ]

parseType :: Parser Type
parseType = makeExprParser factor
  [ [ InfixL (return TApp) ]
  , [ InfixR ((~>) <$ symbol "->") ]
  ]
  where
    factor = M.choice
      [ parseNameType
      , TLam <$ symbol "\\" <*> parseTVar <* symbol ":" <*> parseKind <* symbol "." <*> parseType
      , (TConst . Label . Lab) <$ symbol "'" <*> parseName []
      , parens parseType
      ]

parseTerm :: Parser Term
parseTerm = do
  func <- factor
  args <- M.many argument
  return $ foldl (\f -> either (AppT f) (App f)) func args
  where
    argument = M.eitherP (squares parseType) factor

    factor = M.choice
      [ parseNameTerm
      , Lam <$ symbol "\\" <*> parseVar <* symbol ":" <*> parseType <* symbol "." <*> parseTerm
      , LamT <$ symbol "/\\" <*> parseTVar <* symbol ":" <*> parseKind <* symbol "." <*> parseTerm
      , parens parseTerm
      ]

parseFile :: Text -> Either (M.ParseErrorBundle Text Void) Term
parseFile = M.runParser (spaces >> parseTerm <* M.eof) "test.lune"
