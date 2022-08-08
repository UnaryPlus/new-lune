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

data Term
  = Var Var
  | Label Label
  | Get Term Label
  | Lam (NonEmpty Param) Term
  | Let (NonEmpty Def) Term
  | App Term Term
  | AppT Term (Maybe TVar, Type)
  | InfixApp Var Term Term
