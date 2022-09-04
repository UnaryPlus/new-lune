{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
module Compiler.Translate where

import Data.List.NonEmpty (toList)

import qualified Syntax.First as F
import Syntax.Second

class Translate a b | a -> b, b -> a where
  translate :: a -> b

instance Translate F.TVar TVar where
  translate (F.TV s) = TV s

typeAbst :: (TParam -> Type -> Type) -> F.TParam -> Type -> Type
typeAbst lam (as, k) t =
  foldr (\a -> lam (translate a, k)) t as

lambda :: F.Param -> Term -> Term
lambda p e = case p of
  F.TypeParam (as, k) ->
    foldr (\a -> LamT (translate a, k)) e as
  F.TermParam (xs, t) ->
    foldr (\x -> Lam (x, fmap translate t)) e xs

annotateType :: Type -> Maybe Kind -> Type
annotateType t = \case
  Nothing -> t
  Just k -> TApp (TLam ("a", Just k) "a") t

annotateTerm :: Term -> Maybe Type -> Term
annotateTerm e = \case
  Nothing -> e
  Just t -> App (Lam ("x", Just t) "x") e

expandEnv :: F.Term -> F.Env -> Term
expandEnv r (items, m) = let
  r' = translate r
  field l = Get r' (Lab l)
  end = maybe (field "end") translate m
  cons = \case
    F.Cons x Nothing -> app2 (field "cons") (translate x)
    F.Cons x (Just y) -> app3 (field "cons2") (translate x) (translate y)
    F.Shorthand x -> app3 (field "cons2") (Label (Lab x)) (Var (V x))
  in foldr cons end items

instance Translate F.Def Def where
  translate = \case
    F.TypeDef a ps k t -> let
      t' = annotateType (translate t) k
      in TypeDef (translate a) (foldr (typeAbst TLam) t' ps)
    F.TermDef x ps t e -> let
      e' = annotateTerm (translate e) (fmap translate t)
      in TermDef x (foldr lambda e' ps)

instance Translate F.Type Type where
  translate = \case
    F.TVar a -> TVar (translate a)
    F.TLabel l -> TLabel l
    F.TLam ps t -> foldr (typeAbst TLam) (translate t) ps
    F.TForall ps t -> foldr (typeAbst TForall) (translate t) ps
    F.TApp t1 t2 -> TApp (translate t1) (translate t2)
    F.TInfixApp a t1 t2 -> let
      (a', t1', t2') = (translate a, translate t1, translate t2)
      in TApp (TApp (TVar a') t1') t2'

instance Translate F.Term Term where
  translate = \case
    F.Var x -> Var x
    F.Label l -> Label l
    F.Lam ps e -> foldr lambda (translate e) ps
    F.Let defs e -> Let (translate <$> toList defs) (translate e)
    F.Get e l -> Get (translate e) l
    F.Env e env -> expandEnv e env
    F.App f e -> App (translate f) (translate e)
    F.AppT f (a, t) -> AppT (translate f) (fmap translate a, translate t)
    F.InfixApp x e1 e2 -> app2 (Var x) (translate e1) (translate e2)
