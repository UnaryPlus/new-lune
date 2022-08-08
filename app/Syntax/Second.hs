module Syntax.Second
  ( module Syntax.Common
  , TVar(..)
  , TParam
  , Param
  , Def(..)
  , Type(..)
  , Term(..)
  ) where

import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)

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
