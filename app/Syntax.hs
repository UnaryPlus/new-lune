{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyCase #-}
module Syntax where

import Data.String (IsString(fromString))

newtype Label = Lab String
  deriving (Eq)

data TVar
  = TV String
  | TVF Int
  deriving (Eq, Ord)

newtype Var = V String
  deriving (Eq, Ord)

data Kind
  = KType
  | KRow
  | KLabel
  | KArrow Kind Kind
  deriving (Eq)

data Type
  = TConst TConst
  | TVar TVar
  | TLam TVar Kind Type
  | TApp Type Type
  deriving (Eq)

data TConst
  = Forall Kind
  | Arrow
  | Label Label
  | Nil
  | Cons
  | Record
  | Variant
  | InfRecord
  | InfVariant
  deriving (Eq)

data Term
  = Const Const
  | Var Var
  | Lam Var Type Term
  | App Term Term
  | LamT TVar Kind Term
  | AppT Term Type

data Const
  = Unit
  | Project
  | Delete
  | Construct
  | Absurd
  | Inject
  | Embed
  | Destruct

infixr 9 ~>
class Function a where
  (~>) :: a -> a -> a

instance Function Kind where
  (~>) = KArrow

instance Function Type where
  (~>) t1 t2 = TApp (TApp (TConst Arrow) t1) t2

instance IsString TVar where
  fromString = TV . fromString

instance IsString Type where
  fromString = TVar . fromString

forall :: TVar -> Kind -> Type -> Type
forall a k t = TApp (TConst (Forall k)) (TLam a k t)

pattern TApp2 :: Type -> Type -> Type -> Type
pattern TApp2 t1 t2 t3 = TApp (TApp t1 t2) t3

pattern TApp3 :: Type -> Type -> Type -> Type -> Type
pattern TApp3 t1 t2 t3 t4 = TApp (TApp2 t1 t2 t3) t4

data Error
  = UndefinedT TVar
  | Undefined Var
  | NotKArrow Kind
  | NotArrow Type
  | NotKType Type
  | NotForall Type
  | KindMismatch Kind Kind
  | TypeMismatch Type Type
  | MissingLabel Type

parensIf :: Bool -> String -> String
parensIf b s
  | b = "(" ++ s ++ ")"
  | otherwise = s

class Pretty a where
  pretty :: a -> String

instance Pretty Label where
  pretty (Lab s) = "'" ++ s

instance Pretty TVar where
  pretty = \case
    TV s -> s
    TVF i -> "'" ++ show i

instance Pretty Var where
  pretty (V s) = s

instance Pretty Kind where
  pretty = prettyP False
    where
      prettyP arrowLeft = \case
        KType -> "Type"
        KRow -> "Row"
        KLabel -> "Label"
        KArrow k1 k2 -> parensIf arrowLeft $
          prettyP True k1 ++ "->" ++ prettyP False k2

instance Pretty TConst where
  pretty = \case
    Forall k -> "forall [" ++ pretty k ++ "]"
    Arrow -> "arrow"
    Label s -> pretty s
    Nil -> "nil"
    Cons -> "cons"
    Record -> "record"
    Variant -> "variant"
    InfRecord -> "infrecord"
    InfVariant -> "infvariant"

data TypeLoc
  = ArrowLeft
  | ArrowRight
  | AppLeft
  | AppRight
  | Outer
  deriving (Eq)

instance Pretty Type where
  pretty = prettyP Outer
    where
      prettyP loc = \case
        TConst c -> pretty c
        TVar a -> pretty a
        TLam a k t -> parensIf (loc `elem` [ArrowLeft, AppLeft, AppRight]) $
          "\\" ++ pretty a ++ ":" ++ pretty k ++ ". " ++ prettyP Outer t
        TApp2 (TConst Arrow) t1 t2 -> parensIf (loc `elem` [ArrowLeft, AppLeft, AppRight]) $
          prettyP ArrowLeft t1 ++ " -> " ++ prettyP ArrowRight t2
        TApp t1 t2 -> parensIf (loc == AppRight) $
          prettyP AppLeft t1 ++ " " ++ prettyP AppRight t2

instance Pretty Error where
  pretty = \case
    UndefinedT a -> "type variable " ++ pretty a ++ " is undefined"
    Undefined x -> "variable" ++ pretty x ++ " is undefined"
    NotKArrow k -> "expected a function kind:\n* " ++ pretty k
    NotArrow t -> "expected a function type:\n* " ++ pretty t
    NotKType t -> "expected a type of kind Type:\n* " ++ pretty t
    NotForall t -> "expected a forall type:\n* " ++ pretty t
    KindMismatch k1 k2 -> "could not match kinds:\n* " ++ pretty k1 ++ "\n* " ++ pretty k2
    TypeMismatch t1 t2 -> "could not match types:\n* " ++ pretty t1 ++ "\n* " ++ pretty t2
    MissingLabel t ->  "missing label:\n* " ++ pretty t
