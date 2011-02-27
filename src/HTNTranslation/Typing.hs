{-# LANGUAGE
    FlexibleContexts,
    FlexibleInstances,
    MultiParamTypeClasses,
    OverlappingInstances,
    RankNTypes,
    StandaloneDeriving,
    TypeOperators,
    UndecidableInstances
  #-}
module HTNTranslation.Typing (
    TypeAspect(..), FullType(..), TaskType,
    tinject, baseType, addType, isType,
    TypeMap,
    findTypes, Typer,
    
    CallSpots(..), callSpot, callSpotTyper,
    Primitiveness(..), primitive, primitiveTyper,
    LastPosition(..), isLast, notLast, lastPositionTyper,
    ParentPosition(..), pIsLast, pNotLast, parentPositionTyper
) where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import HTNTranslation.HTNPDDL


type TaskType = FullType 
    (CallSpots ::+:: LastPosition ::+:: ParentPosition ::+:: Primitiveness)
type TypeMap = Map String TaskType

findTypes :: (
    HasTaskHead [StdTaskDef] d,
    HasActions m d,
    HasTaskLists m) =>
    [TypeMap -> d -> TypeMap] -> 
    d -> TypeMap
findTypes typers d =
    let 
        initTM : iterTMs = iterate applyOnce $
            Map.fromList [ (taskName t, baseType) | t <- getTaskHead d]
    in
    untilSame initTM iterTMs
    where
        applyOnce :: TypeMap -> TypeMap
        applyOnce tm = foldl (\tm' f -> f tm' d) tm typers
        untilSame :: TypeMap -> [TypeMap] -> TypeMap
        untilSame x [] = x
        untilSame x (h : tl)
            | x == h = x
            | otherwise = untilSame h tl
    

updateMap :: TypeAspect t => String -> (FullType t) -> Map String (FullType t) 
    -> Map String (FullType t)
updateMap task t =
    Map.alter tadd task
    where
        tadd Nothing = Just t
        tadd (Just t') = Just $ addType t t' 

type Typer m = TypeMap -> m -> TypeMap


applyMTyper :: (HasActions m d) =>
    Typer m -> Typer d
applyMTyper f tm =
    foldl f tm . getActions

class TypeAspect t where
    baseType' :: t e
    addType' :: t e -> t e -> t e
    isType' :: t e -> t e -> Bool

newtype FullType t = FTIn (t (FullType t))
deriving instance (Show (t (FullType t))) => Show (FullType t)
deriving instance (Eq (t (FullType t))) => Eq (FullType t)


addType :: TypeAspect f => FullType f -> FullType f -> FullType f
addType (FTIn f1) (FTIn f2) = FTIn $ addType' f1 f2
isType :: TypeAspect f => FullType f -> FullType f -> Bool
isType (FTIn f1) (FTIn f2) = isType' f1 f2
baseType :: TypeAspect f => FullType f
baseType = FTIn baseType'

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


instance TypeAspect f => (::<::) f f where
    tinj = id
    
instance (TypeAspect f, TypeAspect g) => (::<::) f (f ::+:: g) where
    tinj = flip TIn baseType'

instance (TypeAspect f, TypeAspect g, TypeAspect h, (::<::) f g) 
    => (::<::) f (h ::+:: g) where
    tinj = TIn baseType' . tinj
    

data CallSpots e = 
    CallSpots  (Set (String, Int)) 
    deriving (Eq, Show)  
instance TypeAspect CallSpots where
    baseType' = CallSpots (Set.empty)
    isType' (CallSpots t1) (CallSpots t2) = t1 `Set.isSubsetOf` t2 
    addType' (CallSpots t1) (CallSpots t2) =
        CallSpots $ Set.union t1 t2
callSpot :: (CallSpots ::<:: f) =>
    String -> Int -> FullType f
callSpot caller position =
    tinject $ CallSpots $ Set.singleton (caller, position)

callSpotMTyper ::
    (HasName m, HasTaskLists m) =>
    Typer m
callSpotMTyper initTM m =
    foldl addCaller initTM $ enumerateTasks m
    where
    addCaller tm (n, task) =
        updateMap (taskName task) (callSpot (getName m) n) tm

callSpotTyper ::
    (HasName m, HasTaskLists m, HasActions m d) => Typer d
callSpotTyper = applyMTyper callSpotMTyper


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
primitive :: forall f . (Primitiveness ::<:: f) => FullType f 
primitive = tinject PrimitiveTask

primitiveMTyper :: (HasTaskHead StdTaskHead m, HasTaskLists m) => Typer m
primitiveMTyper tm m = case (getTaskHead m, getTaskLists m) of
    (Nothing, _) -> tm
    (_, []) -> tm
    (Just task, _) ->
        updateMap (taskName task) (tinject NonPrimitiveTask) tm

primitiveTyper :: 
    (HasTaskHead StdTaskHead m, HasTaskLists m, HasActions m d) 
    => Typer d
primitiveTyper = applyMTyper primitiveMTyper

data LastPosition e =
    NoPosition
    | NotLastPosition
    | LastPosition
    | BothPosition
    deriving (Eq, Show)
instance TypeAspect LastPosition where
    baseType' = NoPosition
    isType' NoPosition _ = True
    isType' _ BothPosition = True
    isType' NotLastPosition NotLastPosition = True
    isType' LastPosition LastPosition = True
    isType' _ _ = False
    addType' NoPosition x = x
    addType' x NoPosition = x
    addType' NotLastPosition NotLastPosition = NotLastPosition
    addType' LastPosition LastPosition = LastPosition
    addType' _ _ = BothPosition
noPosition :: forall f . (LastPosition ::<:: f) => FullType f
noPosition = tinject NoPosition
isLast :: forall f . (LastPosition ::<:: f) => FullType f
isLast = tinject LastPosition        
notLast :: forall f . (LastPosition ::<:: f) => FullType f
notLast = tinject NotLastPosition

lastPositionMTyper :: (HasTaskLists m, 
    HasTaskConstraints m) => Typer m
lastPositionMTyper initTM m = 
    let
        subtasks = enumerateTasks m
        lastTask = findLastTask m
        nontail = maybe subtasks (\x -> delete x subtasks)  lastTask
        tm' = flip (maybe initTM) lastTask $
            \(_, task) -> 
                updateMap (taskName task) isLast initTM
    in
    foldl (\tm (_, task) -> updateMap (taskName task) notLast tm) tm' nontail
    
lastPositionTyper :: (HasTaskLists m,
    HasTaskConstraints m,
    HasActions m d) =>
    Typer d
lastPositionTyper = applyMTyper lastPositionMTyper

data ParentPosition e =
    PNoPosition
    | PNotLastPosition
    | PLastPosition
    | PBothPosition
    deriving (Eq, Show)
instance TypeAspect ParentPosition where
    baseType' = PNoPosition
    isType' PNoPosition _ = True
    isType' _ PBothPosition = True
    isType' PNotLastPosition PNotLastPosition = True
    isType' PLastPosition PLastPosition = True
    isType' _ _ = False
    addType' PNoPosition x = x
    addType' x PNoPosition = x
    addType' PNotLastPosition PNotLastPosition = PNotLastPosition
    addType' PLastPosition PLastPosition = PLastPosition
    addType' _ _ = PBothPosition
pNoPosition :: forall f . (ParentPosition ::<:: f) => FullType f
pNoPosition = tinject PNoPosition    
pIsLast :: forall f . (ParentPosition ::<:: f) => FullType f
pIsLast = tinject PLastPosition        
pNotLast :: forall f . (ParentPosition ::<:: f) => FullType f
pNotLast = tinject PNotLastPosition
pBothPosition :: forall f . (ParentPosition ::<:: f) => FullType f
pBothPosition = tinject PBothPosition


parentPositionMTyper :: (HasTaskLists m, 
    HasTaskHead StdTaskHead m) => Typer m
parentPositionMTyper initTM m = 
    let
        taskt = maybe noPosition id $ do
            task <- getTaskHead m
            Map.lookup (taskName task) initTM
        subt :: TaskType
        subt = if isType taskt noPosition then pNoPosition
            else if isType taskt isLast then pIsLast
            else if isType taskt notLast then pNotLast
            else pBothPosition
        subtasks = enumerateTasks m
    in
    foldl (\tm (_, task) -> updateMap (taskName task) subt tm) initTM subtasks
    
parentPositionTyper :: (HasTaskLists m,
    HasTaskHead StdTaskHead m,
    HasActions m d) =>
    Typer d
parentPositionTyper = applyMTyper parentPositionMTyper
