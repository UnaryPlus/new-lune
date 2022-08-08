module Syntax.Frontend where

import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)

import Syntax.Common

newtype Var = V Text

newtype TVar = TV Text

type TParam = (NonEmpty TVar, Maybe Kind)

data Param
  = TypeParam (NonEmpty TVar, Maybe Kind)
  | TermParam (NonEmpty Var, Maybe Type)

data Def
  = TypeDef TVar [TParam] (Maybe Kind) Type
  | TermDef Var [Param] (Maybe Type) Term

data Type
  = TVar TVar
  | TLabel Label
  | TLam (NonEmpty TParam) Type
  | TForall (NonEmpty TParam) Type
  | TApp Type Type
  | TInfixApp TVar Type Type

data EnvItem
  = Cons Term (Maybe Term)
  | Shorthand Text

type Env = (NonEmpty EnvItem, Maybe Term)

data Term
  = Var Var
  | Label Label
  | Lam (NonEmpty Param) Term
  | Let (NonEmpty Def) Term
  | Get Term Label
  | Env Term Env
  | App Term Term
  | AppT Term (Maybe TVar, Type)
  | InfixApp Var Term Term
