{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Compiler.Infer (runInfer) where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad ((>=>))

import Control.Monad.State (MonadState, evalStateT)
import qualified Control.Monad.State as State
import Control.Monad.Except (MonadError, throwError)

import Syntax.Common
import Syntax.Core

constKind :: TConst -> Kind
constKind = \case
  Forall k -> (k ~> KType) ~> KType
  Arrow -> KType ~> KType ~> KType
  Label _ -> KLabel
  Nil -> KRow
  Cons -> KLabel ~> KType ~> KRow ~> KRow
  Record -> KRow ~> KType
  Variant -> KRow ~> KType
  InfRecord -> (KType ~> KRow) ~> KType
  InfVariant -> (KType ~> KRow) ~> KType

constType :: Const -> Type
constType = \case
  Unit -> record (TConst Nil)
  Project -> forall_arl $ record cons_lar ~> "a"
  Delete -> forall_arl $ record cons_lar ~> record "r"
  Construct -> forall_arl $ "a" ~> record "r" ~> record cons_lar
  Absurd -> forall "a" KType $ variant (TConst Nil) ~> "a"
  Inject -> forall_arl $ "a" ~> variant cons_lar
  Embed -> forall_arl $ variant cons_lar ~> "a"
  Destruct -> forall "b" KType $ forall_arl $
    ("a" ~> "b") ~> (variant "r" ~> "b") ~> variant cons_lar ~> "b"
  where
    forall_arl = forall "a" KType . forall "r" KRow . forall "l" KLabel
    cons_lar = TApp3 (TConst Cons) "l" "a" "r"
    record = TApp (TConst Record)
    variant = TApp (TConst Variant)

lookupKind :: MonadError Error m => TVar -> Map TVar Kind -> m Kind
lookupKind a env =
  case Map.lookup a env of
    Just k -> return k
    Nothing -> throwError (UndefinedT a)

matchKArrow :: MonadError Error m => Kind -> m (Kind, Kind)
matchKArrow = \case
  KArrow k1 k2 -> return (k1, k2)
  k -> throwError (NotKArrow k)

inferKind :: MonadError Error m => Map TVar Kind -> Type -> m Kind
inferKind env = \case
  TConst c -> return (constKind c)
  TVar a -> lookupKind a env
  TLam a k t -> do
    k_t <- inferKind (Map.insert a k env) t
    return (k ~> k_t)
  TApp s t -> do
    (k1, k2) <- matchKArrow =<< inferKind env s
    k_t <- inferKind env t
    if k_t == k1 then return k2
      else throwError (KindMismatch k_t k1)

verifyKType :: MonadError Error m => Map TVar Kind -> Type -> m ()
verifyKType env t = do
  k <- inferKind env t
  case k of
    KType -> return ()
    _ -> throwError (NotKType t)

lookupType :: MonadError Error m => Var -> Map Var Type -> m Type
lookupType x env =
  case Map.lookup x env of
    Just t -> return t
    Nothing -> throwError (Undefined x)

matchArrow :: (MonadError Error m, MonadState Int m) => Type -> m (Type, Type)
matchArrow = whnf >=> \case
  TApp (TApp (TConst Arrow) t1) t2 -> return (t1, t2)
  t -> throwError (NotArrow t)

matchForall :: (MonadError Error m, MonadState Int m) => Type -> m (Kind, Type)
matchForall = whnf >=> \case
  TApp (TConst (Forall k)) t -> return (k, t)
  t -> throwError (NotForall t)

inferType :: (MonadError Error m, MonadState Int m) => Map Var Type -> Map TVar Kind -> Term -> m Type
inferType env kenv = \case
  Const c -> return (constType c)
  Var x -> lookupType x env
  Lam x t e -> do
    verifyKType kenv t
    t_e <- inferType (Map.insert x t env) kenv e
    return (t ~> t_e)
  App f e -> do
    (t1, t2) <- matchArrow =<< inferType env kenv f
    t_e <- inferType env kenv e
    verifyEquiv t1 t_e
    return t2
  LamT a k e -> do
    t <- inferType env (Map.insert a k kenv) e
    return (forall a k t)
  AppT f t -> do
    (k, t_f) <- matchForall =<< inferType env kenv f
    k_t <- inferKind kenv t
    if k == k_t
      then return (TApp t_f t)
      else throwError (KindMismatch k k_t)

verifyEquiv :: (MonadError Error m, MonadState Int m) => Type -> Type -> m ()
verifyEquiv t1 t2 = do
  i <- State.get
  t1' <- reduce t1
  t2' <- reduce t2
  equivalent t1' t2'
  State.put i

matchCons :: MonadError Error m => Type -> Type -> m (Type, Type)
matchCons l = \case
  TApp3 (TConst Cons) l' v r
    | l' == l -> return (v, r)
    | otherwise -> do
      (v_r, r_r) <- matchCons l r
      return (v_r, TApp3 (TConst Cons) l' v r_r)
  _ -> throwError (MissingLabel l)

equivalent :: (MonadError Error m, MonadState Int m) => Type -> Type -> m ()
equivalent = curry \case
  (TConst c1, TConst c2) | c1 == c2 -> return ()
  (TVar a, TVar b) | a == b -> return ()
  (TApp3 (TConst Cons) l v1 r1, t2) -> do
    (v2, r2) <- matchCons l t2
    equivalent v1 v2
    equivalent r1 r2
  -- inf-record f = record (f (inf-record f))
  (t1@(TApp (TConst InfRecord) f), TApp (TConst Record) t) ->
    flip equivalent t =<< beta f t1
  (TApp (TConst Record) t, t2@(TApp (TConst InfRecord) f)) ->
    equivalent t =<< beta f t2
  -- inf-variant f = variant (f (inf-variant f))
  (t1@(TApp (TConst InfVariant) f), TApp (TConst Variant) t) ->
    flip equivalent t =<< beta f t1
  (TApp (TConst InfVariant) t, t2@(TApp (TConst Variant) f)) ->
    equivalent t =<< beta f t2
  (TLam a1 _ t1, TLam a2 _ t2) -> do
    a' <- fresh
    let t1' = rename a1 a' t1
    let t2' = rename a2 a' t2
    equivalent t1' t2'
  (TApp f1 t1, TApp f2 t2) -> equivalent f1 f2 >> equivalent t1 t2
  (t1, t2) -> throwError (TypeMismatch t1 t2)

fresh :: MonadState Int m => m TVar
fresh = do
  i <- State.get
  State.modify (1+)
  return (TVF i)

beta :: MonadState Int m => Type -> Type -> m Type
beta t1 t2 = case t1 of
  TLam a _ t -> subst a t2 t
  _ -> return (TApp t1 t2)

whnf :: MonadState Int m => Type -> m Type
whnf = \case
  TApp t1 t2 -> whnf t1 >>= \case
    TLam a _ t -> whnf =<< subst a t2 t
    t1' -> return (TApp t1' t2)
  t -> return t

reduce :: MonadState Int m => Type -> m Type
reduce = \case
  TConst c -> return (TConst c)
  TVar a -> return (TVar a)
  TLam a k t -> TLam a k <$> reduce t
  TApp t1 t2 -> reduce t1 >>= \case
    TLam a _ t -> reduce =<< subst a t2 t
    t1' -> TApp t1' <$> reduce t2

subst :: MonadState Int m => TVar -> Type -> Type -> m Type
subst b t_b = \case
  TConst c -> return (TConst c)
  TVar a -> return (if a == b then t_b else TVar a)
  TLam a k t
    | a == b -> return (TLam a k t)
    | a `freeIn` t_b -> do
        a' <- fresh
        let t' = rename a a' t
        TLam a' k <$> sub t'
    | otherwise -> TLam a k <$> sub t
  TApp t1 t2 -> TApp <$> sub t1 <*> sub t2
  where sub = subst b t_b

rename :: TVar -> TVar -> Type -> Type
rename b b' = \case
  TConst c -> TConst c
  TVar a -> TVar (if a == b then b' else a)
  TLam a k t
    | a == b -> TLam a k t
    | otherwise -> TLam a k (ren t)
  TApp t1 t2 -> TApp (ren t1) (ren t2)
  where ren = rename b b'

freeIn :: TVar -> Type -> Bool
freeIn b = \case
  TConst _ -> False
  TVar a -> a == b
  TLam a _ t -> a /= b && b `freeIn` t
  TApp t1 t2 -> b `freeIn` t1 || b `freeIn` t2

runInfer :: Term -> Either Error Type
runInfer e = evalStateT (reduce =<< inferType Map.empty Map.empty e) 0
