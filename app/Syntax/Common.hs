module Syntax.Common where

import Data.Text (Text)

newtype Label = Lab Text
  deriving (Eq)

data Kind
  = KType
  | KRow
  | KLabel
  | KArrow Kind Kind
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

data Const
  = Unit
  | Project
  | Delete
  | Construct
  | Absurd
  | Inject
  | Embed
  | Destruct
