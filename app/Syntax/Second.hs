module Syntax.Second
  ( module Syntax.Common
  , TVar(..)
  , TParam
  , Param
  , Def(..)
  , Type(..)
  , Term(..)
  , app2
  , app3
  ) where

import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)
import Data.String (IsString(fromString))

import Syntax.Common

newtype TVar = TV Text

type TParam = (TVar, Maybe Kind)

type Param = (Var, Maybe Type)

data Def
  = TypeDef TVar Type
  | TermDef Var Term

data Type
  = TVar TVar
  | TLabel Label
  | TLam TParam Type
  | TForall TParam Type
  | TApp Type Type

data Term
  = Var Var
  | Label Label
  | Lam Param Term
  | LamT TParam Term
  | Let (NonEmpty Def) Term
  | Get Term Label
  | App Term Term
  | AppT Term (Maybe TVar, Type)

app2 :: Term -> Term -> Term -> Term
app2 f x = App (App f x)

app3 :: Term -> Term -> Term -> Term -> Term
app3 f x y = App (app2 f x y)

instance IsString TVar where
  fromString = TV . fromString

instance IsString Type where
  fromString = TVar . fromString

instance IsString Term where
  fromString = Var . fromString
