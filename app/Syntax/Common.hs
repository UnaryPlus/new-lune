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

infixr 9 ~>
class Function a where
  (~>) :: a -> a -> a

instance Function Kind where
  (~>) = KArrow
