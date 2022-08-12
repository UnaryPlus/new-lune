module Syntax.Common where

import Data.Text (Text)
import Data.String (IsString(fromString))

newtype Var = V Text
  deriving (Eq, Ord)

instance IsString Var where
  fromString = V . fromString

newtype Label = Lab Text
  deriving (Eq)

data Kind
  = KType
  | KRow
  | KLabel
  | KArrow Kind Kind
  deriving (Eq)
