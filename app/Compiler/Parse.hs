{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse {-(parseFile)-} where

import Text.Megaparsec (Parsec, chunk, notFollowedBy, satisfy, try, takeWhileP, takeWhile1P)
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Monad.Combinators ((<|>), between, choice, optional, many, eitherP)
import Control.Monad.Combinators.NonEmpty (some)
import Control.Monad.Combinators.Expr

import Data.Void (Void)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (isAsciiUpper, isAsciiLower, isDigit)

import Syntax.Common
import Syntax.Frontend

type Parser = Parsec Void Text

spaces :: Parser ()
spaces = L.space C.space1
  (L.skipLineComment "--")
  (L.skipBlockCommentNested "[-" "-]")

symbol :: Text -> Parser ()
symbol s = chunk s >> spaces

parens, squares, backticks, quotes :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
squares = between (symbol "[") (symbol "]")
backticks = between (symbol "`") (symbol "`")
quotes = between (symbol "'") (symbol "'")

infixChar, prefixHead, prefixTail :: Char -> Bool
infixChar c = c `elem` ("~!@#$%^&*-=+\\|;:<>/?" :: [Char])
prefixHead c = isAsciiUpper c || isAsciiLower c
prefixTail c = prefixHead c || infixChar c || isDigit c

reservedInfix :: Text -> Parser ()
reservedInfix s = do
  _ <- chunk s
  notFollowedBy (satisfy infixChar)
  spaces

reservedPrefix :: Text -> Parser ()
reservedPrefix s = do
  _ <- chunk s
  notFollowedBy (satisfy prefixTail)
  spaces

infixName :: Parser Text
infixName = do
  let keywords = [ "\\", "=", ":" ]
  notFollowedBy (choice (map reservedInfix keywords))
  s <- takeWhile1P Nothing infixChar
  spaces
  return s

prefixName :: Parser Text
prefixName = do
  let keywords = [ "type", "func", "let", "in", "forall" ]
  notFollowedBy (choice (map reservedPrefix keywords))
  c <- satisfy prefixHead
  s <- takeWhileP Nothing prefixTail
  spaces
  return (Text.cons c s)

anyName :: Parser Text
anyName = infixName <|> prefixName

infixOp :: Parser Text
infixOp = infixName <|> backticks prefixName

annotation :: Parser a -> Parser (Maybe a)
annotation p = optional (reservedInfix ":" >> p)

parseKind :: Parser Kind
parseKind = makeExprParser factor
  [ [ InfixR ((~>) <$ reservedInfix "->") ] ]
  where
    factor = choice
      [ KType <$ reservedPrefix "Type"
      , KRow <$ reservedPrefix "Row"
      , KLabel <$ reservedPrefix "Label"
      , parens parseKind
      ]

generalParam
  :: (Text -> var)
  -> Parser ty
  -> (forall a. Parser a -> Parser a)
  -> Bool
  -> Parser (NonEmpty var, Maybe ty)
generalParam makeVar ty brackets required
  | required = inBrackets
  | otherwise = inBrackets <|> bare
  where
    var = makeVar <$> anyName
    bare = (\x -> (x :| [], Nothing)) <$> var
    inBrackets = brackets ((,) <$> some var <*> annotation ty)

parseTParam :: Parser TParam
parseTParam = generalParam TV parseKind parens False

parseParam :: Parser Param
parseParam = choice
  [ TypeParam <$> generalParam TV parseKind squares True
  , TermParam <$> generalParam V parseType parens False
  ]

chain :: Parser (a -> b -> a) -> Parser a -> Parser b -> Parser a
chain op left right = apply =<< left
  where
    apply l =
      do { f <- op; r <- right; apply (f l r) }
      <|> return l

parseType :: Parser Type
parseType = makeExprParser factor
  [ [ InfixR (TInfixApp . TV <$> infixOp) ] ]
  where
    factor = chain (return TApp)
      (appFactor anyName)
      (appFactor prefixName)

    appFactor name = choice
      [ TVar . TV <$> name
      , TLabel . Lab <$> quotes anyName
      , TLam <$ reservedInfix "\\" <*> some parseTParam <* symbol "." <*> parseType
      , TForall <$ reservedPrefix "forall" <*> some parseTParam <* symbol "." <*> parseType
      , parens parseType
      ]

parseTerm :: Parser Term
parseTerm = makeExprParser factor
  [ [ InfixR (InfixApp . V <$> infixOp) ] ]
  where
    factor = chain (return \f -> either (App f) (AppT f))
      (appFactor anyName)
      (eitherP (appFactor prefixName) typeArg)

    appFactor name = chain (Get <$ symbol ".")
      (getFactor name)
      (Lab <$> anyName)

    getFactor name = choice
      [ Var . V <$> name
      , Label . Lab <$> quotes anyName
      , Lam <$ reservedInfix "\\" <*> some parseParam <* symbol "." <*> parseTerm
      , Let <$ reservedPrefix "let" <*> some parseDef <* reservedPrefix "in" <*> parseTerm
      , parens parseTerm
      ]

    explicit a t = (Just a, t)
    implicit t = (Nothing, t)

    typeArg = squares $ choice
      [ try (explicit <$> (TV <$> anyName) <* reservedInfix "=" <*> parseType)
      , implicit <$> parseType
      ]

parseDef :: Parser Def
parseDef = choice
  [ TypeDef <$ reservedPrefix "type" <*> (TV <$> anyName) <*> many parseTParam
    <*> annotation parseKind <* reservedInfix "=" <*> parseType
  , TermDef <$ reservedPrefix "func" <*> (V <$> anyName) <*> many parseParam
    <*> annotation parseType <* reservedInfix "=" <*> parseTerm
  ]
