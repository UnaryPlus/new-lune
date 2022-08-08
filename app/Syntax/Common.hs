module Syntax.Common where

import Data.Text (Text)

newtype Var = V Text
  deriving (Eq, Ord)

newtype Label = Lab Text
  deriving (Eq)

data Kind
  = KType
  | KRow
  | KLabel
  | KArrow Kind Kind
  deriving (Eq)
