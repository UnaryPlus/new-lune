module Syntax.Frontend where

import Data.Text (Text)

import Syntax.Common

newtype Var = V Text

newtype TVar = TV Text

data Fixity
  = Infix Double
  | Infixl Double
  | Infixr Double

data Param
  = TypeParam TVar Kind
  | TermParam Var Type

data Arg
  = TypeArg (Maybe TVar) Type
  | TermArg Term

data Def
  = TypeDef (Maybe Fixity) TVar [(TVar, Kind)] (Maybe Kind) Type
  | FuncDef (Maybe Fixity) Var [Param] (Maybe Type) Term

data Type
  = TConst TConst
  | TVar TVar
  | TLam [(TVar, Kind)] Type
  | TApp [Type]

data Term
  = Const Const
  | Var Var
  | Lam [Param] Term
  | App Term [Arg]
