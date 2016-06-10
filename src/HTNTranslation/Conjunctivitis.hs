{-# OPTIONS_GHC
    -fcontext-stack=30
    -Wall
  #-}
{-# LANGUAGE
    FlexibleContexts,
    FlexibleInstances,
    MultiParamTypeClasses,
    RankNTypes,
    ScopedTypeVariables,
    TypeOperators,
    UndecidableInstances
  #-}
-- Module for performing FD-style canonicalization
module HTNTranslation.Conjunctivitis
where

import Control.Monad.State
import Planning.Expressions
import Planning.PDDL.PDDL3_0
import Planning.Util

-- Modified GD restricted to the FDNNF classes above
type GD1 = Atomic TermExpr :+: Not :+: And :+: Or :+: Exists TypedVarExpr
type GD2 = Atomic TermExpr :+: Not :+: And :+: Or



-- Partial canonization (Removing implies, forall)
class (Functor f, Functor g) => FDCanonize f g where
    fdcanon' :: f (Expr g) -> Expr g
fdcanon :: (NNF f f, FDCanonize f g, FDNNF g g) => Expr f -> Expr g
fdcanon = fdnnf . foldExpr fdcanon' . nnf


instance (FDCanonize f h, FDCanonize g h) => FDCanonize (f :+: g) h where
    fdcanon' (Inl x) = fdcanon' x
    fdcanon' (Inr y) = fdcanon' y

instance (Atomic t :<: f) => FDCanonize (Atomic t) f where
    fdcanon' (Atomic p tl) = eAtomic p tl

instance (Not :<: f) => FDCanonize Not f where
    fdcanon' (Not e) = eNot e

instance (Or :<: f) => FDCanonize Or f where
    fdcanon' (Or el) = eOr el

instance (And :<: f) => FDCanonize And f where
    fdcanon' (And el) = eAnd el

instance (Not :<: f, Or :<: f) => FDCanonize Imply f where
    fdcanon' (Imply e1 e2) = eOr [(eNot e1), e2]

instance (Exists v :<: f) => FDCanonize (Exists v) f where
    fdcanon' (Exists v e) = eExists v e

instance (Not :<: f, Exists v :<: f) => FDCanonize (ForAll v) f where
    fdcanon' (ForAll v e) = eNot $ eExists v $ eNot e



-- Uses a naming function to transform Exists into derived predicates
class (Functor f, Functor g) => ExistentialEliminator f f' g where
    existsElim :: String -> f (Expr f') -> State [(TypedPredicateExpr, Expr g)] (Expr g)

elimExists :: (ExistentialEliminator f f g) => String -> Expr f -> Expr g
elimExists prefix (In e) = evalState (existsElim prefix e) []


instance (ExistentialEliminator f f' h, ExistentialEliminator g f' h) => ExistentialEliminator (f :+: g) f' h where
    existsElim prefix (Inr x) = existsElim prefix x
    existsElim prefix (Inl x) = existsElim prefix x

instance (Atomic t :<: f, Atomic t :<: g) => ExistentialEliminator (Atomic t) f g where
    existsElim _ (Atomic p tl) = return $ eAtomic p tl

-- instance (Not :<: f, Not :<: g) => ExistentialEliminator Not f g where
--    existsElim prefix (Not e) = existsElim 

-- nnf helper class (modified for FD-style FDNNF (no ForAll)
-- fdnnf' takes a bool arg, False for negated, True for not-negated.
class (Functor f, Functor g) => FDNNF f g where
    fdnnf' :: Bool -> f (Expr g) -> Expr g
fdnnf :: (FDNNF f f) => Expr f -> Expr f
fdnnf (In e) = fdnnf' True e

instance (FDNNF f h, FDNNF g h) => FDNNF (f :+: g) h where
    fdnnf' b (Inr x) = fdnnf' b x
    fdnnf' b (Inl y) = fdnnf' b y

instance (Not :<: g, Atomic t :<: g) => FDNNF (Atomic t) g where
    fdnnf' True (Atomic p tl) = eAtomic p tl
    fdnnf' False (Atomic p tl) = eNot $ eAtomic p tl

instance (FDNNF g g) => FDNNF Not g where
    fdnnf' b (Not (In e)) = fdnnf' (not b) e

instance (And :<: g, Or :<: g, FDNNF g g) => FDNNF And g where
    fdnnf' True (And el) = eAnd [fdnnf' True e | In e <-  el]
    fdnnf' False (And el) = eOr [fdnnf' False e | In e <- el]

instance (And :<: g, Or :<: g, FDNNF g g) => FDNNF Or g where
    fdnnf' True (Or el) = eOr [fdnnf' True e | In e <-  el]
    fdnnf' False (Or el) = eAnd [fdnnf' False e | In e <-  el]

instance (Exists t :<: g, Not :<: g, FDNNF g g) => FDNNF (Exists t) g where
    fdnnf' True (Exists vl (In e)) = eExists vl $ fdnnf' True e
    fdnnf' False (Exists vl (In e)) = eNot $ eExists vl $ fdnnf' True e

