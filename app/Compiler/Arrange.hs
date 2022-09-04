{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Compiler.Arrange where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import qualified Data.Bifunctor as Bf
import Data.Foldable (foldrM)

import Syntax.Second

class Ord v => FreeVars a v | a -> v where
  freeVars :: a -> Set v

instance FreeVars Type TVar where
  freeVars = \case
    TVar a -> Set.singleton a
    TLabel _ -> Set.empty
    TLam (a, _) t -> Set.delete a (freeVars t)
    TForall (a, _) t -> Set.delete a (freeVars t)
    TApp t1 t2 -> freeVars t1 <> freeVars t2

instance FreeVars Term Var where
  freeVars = \case
    Var x -> Set.singleton x
    Label _ -> Set.empty
    Lam (x, _) e -> Set.delete x (freeVars e)
    LamT _ e -> freeVars e
    Let defs e -> freeVars e \\ domain defs
    Get e _ -> freeVars e
    App f e -> freeVars f <> freeVars e
    AppT e _ -> freeVars e
    where
      domain = foldr add Set.empty
      add (TypeDef _ _) = id
      add (TermDef x _) = Set.insert x


arrange :: FreeVars a v => [(a, v)] -> Either v [(a, v)]
arrange

insert :: Ord k => (k, a) -> Map k a -> Either k (Map k a)
insert (k, v) m
  | Map.member k m = Left k
  | otherwise = Right (Map.insert k v m)

referenceMap :: FreeVars a v => [(v, a)] -> Either v (Map v (Set v))
referenceMap = foldrM insert Map.empty . map (Bf.second freeVars)
