{-# LANGUAGE
    FlexibleContexts,
    FlexibleInstances,
    MultiParamTypeClasses,
    OverlappingInstances,
    StandaloneDeriving,
    TypeOperators,
    UndecidableInstances
  #-}
module HTNTranslation.Typing (
    TypeAspect(..)
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import HTNTranslation.HTNPDDL





class TypeAspect t where
    baseType' :: t e
    addType' :: t e -> t e -> t e
    isType' :: t e -> t e -> Bool

newtype FullType t = FTIn (t (FullType t))
deriving instance (Show (t (FullType t))) => Show (FullType t)
 
infixr 6 ::+::
data (f ::+:: g) e = TIn (f e) (g e) deriving (Eq, Show)


instance (TypeAspect f, TypeAspect g) => TypeAspect (f ::+:: g) where
    baseType' = TIn baseType' baseType'
    addType' (TIn f1 g1) (TIn f2 g2) = TIn (addType' f1 f2) (addType' g1 g2)
    isType' (TIn f1 g1) (TIn f2 g2) = isType' f1 f2 && isType' g1 g2


class (TypeAspect sub, TypeAspect sup) => sub ::<:: sup where
    tinj :: sub a -> sup a

tinject :: (g ::<:: f) => g (FullType f) -> FullType f
tinject = FTIn . tinj

addType :: TypeAspect f => FullType f -> FullType f -> FullType f
addType (FTIn f1) (FTIn f2) = FTIn $ addType' f1 f2
isType :: TypeAspect f => FullType f -> FullType f -> Bool
isType (FTIn f1) (FTIn f2) = isType' f1 f2
baseType :: TypeAspect f => FullType f
baseType = FTIn baseType'


instance TypeAspect f => (::<::) f f where
    tinj = id
    
instance (TypeAspect f, TypeAspect g) => (::<::) f (f ::+:: g) where
    tinj = flip TIn baseType'

instance (TypeAspect f, TypeAspect g, TypeAspect h, (::<::) f g) 
    => (::<::) f (h ::+:: g) where
    tinj = TIn baseType' . tinj
    

data CallSpots e = 
    NoCallSpots 
    | OneCallSpot 
    | MoreCallSpots 
    deriving (Eq, Ord, Show)  
instance TypeAspect CallSpots where
    baseType' = NoCallSpots
    isType' t1 t2 = t1 <= t2
    addType' t1 t2
        | t1 == t2 = t1
        | t1 < t2 = t2
        | otherwise = t1

data LastPosition e =
    NotLastPosition
    | LastPosition
    deriving (Eq, Ord, Show)
instance TypeAspect LastPosition where
    baseType' = NotLastPosition
    isType' t1 t2 = t1 <= t2
    addType' t1 t2
        | t1 == t2 = t1
        | otherwise = LastPosition

data Primitiveness e =
    PrimitiveTask
    | NonPrimitiveTask
    deriving (Eq, Ord, Show)
instance TypeAspect Primitiveness where
    baseType' = PrimitiveTask
    isType' t1 t2 = t1 <= t2
    addType' t1 t2
        | t1 == t2 = t1
        | otherwise = NonPrimitiveTask


data DecomposesWith e =
    DecomposesWith (Set String)
    deriving (Eq, Show)
instance TypeAspect DecomposesWith where
    baseType' = DecomposesWith (Set.empty)
    isType' (DecomposesWith t1) (DecomposesWith t2) = Set.isSubsetOf t1 t2
    addType' (DecomposesWith t1) (DecomposesWith t2) = 
        DecomposesWith $ Set.union t1 t2
    
type TaskType = CallSpots ::+:: LastPosition ::+:: Primitiveness ::+:: DecomposesWith

test1 :: FullType TaskType
test1 = tinject OneCallSpot
test2 :: FullType TaskType
test2 = tinject LastPosition

