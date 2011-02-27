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
    tinject, tinspect, baseType, addType, extendsType,
    TypeMap,
    findTypes, Typer,
    
    CallSpots(..), callSpot, callSpots, callSpotTyper,
    CallCount(..), callCount, callCounts, callCountTyper,
    Primitiveness(..), primitive, nonPrimitive, primitiveTyper,
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
    (CallSpots ::+:: CallCount ::+:: LastPosition ::+:: ParentPosition ::+:: Primitiveness)
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

class (Functor t) => TypeAspect t where
    baseType' :: t e
    addType' :: t e -> t e -> t e

newtype FullType t = FTIn (t (FullType t))
deriving instance (Show (t (FullType t))) => Show (FullType t)
deriving instance (Eq (t (FullType t))) => Eq (FullType t)


addType :: TypeAspect f => FullType f -> FullType f -> FullType f
addType (FTIn f1) (FTIn f2) = FTIn $ addType' f1 f2
extendsType :: (Eq (FullType f), TypeAspect f) => FullType f -> FullType f -> Bool
extendsType f1 = (== f1) . addType f1
baseType :: TypeAspect f => FullType f
baseType = FTIn baseType'

infixr 6 ::+::
data (f ::+:: g) e = TIn (f e) (g e) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (f ::+:: g) where
    fmap f (TIn f1 g2) = TIn (fmap f f1) (fmap f g2)

instance (TypeAspect f, TypeAspect g) => TypeAspect (f ::+:: g) where
    baseType' = TIn baseType' baseType'
    addType' (TIn f1 g1) (TIn f2 g2) = TIn (addType' f1 f2) (addType' g1 g2)


class (TypeAspect sub, TypeAspect sup) => sub ::<:: sup where
    tinj :: sub a -> sup a
    tinsp :: sup a -> sub a

tinject :: (g ::<:: f) => g (FullType f) -> FullType f
tinject = FTIn . tinj

tinspect :: (g ::<:: f) => FullType f -> g (FullType f)
tinspect (FTIn f) = tinsp f


instance TypeAspect f => (::<::) f f where
    tinj = id
    tinsp = id
    
instance (TypeAspect f, TypeAspect g) => (::<::) f (f ::+:: g) where
    tinj = flip TIn baseType'
    tinsp (TIn f _) = f

instance (TypeAspect f, TypeAspect g, TypeAspect h, (::<::) f g) 
    => (::<::) f (h ::+:: g) where
    tinj = TIn baseType' . tinj
    tinsp (TIn _ g) = tinsp g


data CallSpots e = 
    CallSpots  (Set (String, Int)) 
    deriving (Eq, Show)  
instance Functor CallSpots where
    fmap _ (CallSpots cs) = CallSpots cs
instance TypeAspect CallSpots where
    baseType' = CallSpots (Set.empty)
    addType' (CallSpots t1) (CallSpots t2) =
        CallSpots $ Set.union t1 t2
callSpot :: (CallSpots ::<:: f) =>
    String -> Int -> FullType f
callSpot caller position =
    tinject $ CallSpots $ Set.singleton (caller, position)
callSpots :: (CallSpots ::<:: f) =>
    FullType f -> [(String, Int)]
callSpots = (\(CallSpots cs) -> Set.toList cs) . tinspect


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


data CallCount e =
    CallCount Int
    deriving (Eq, Ord, Show)
instance Functor CallCount where
    fmap _ (CallCount n) = CallCount n
instance TypeAspect CallCount where
    baseType' = CallCount 0
    addType' (CallCount n1) (CallCount n2) = CallCount $ max n1 n2
callCount :: forall f . (CallCount ::<:: f) => Int -> FullType f
callCount = tinject . CallCount
callCounts :: forall f . (CallCount ::<:: f) => FullType f -> Int
callCounts = (\(CallCount n) -> n) . tinspect 

callCountTyper :: Typer d
callCountTyper tm _ =
    flip Map.mapWithKey tm 
    (\ task t -> addType t $ callCount $ (\(CallSpots cs) -> Set.size cs) $ tinspect t)
    

data Primitiveness e =
    PrimitiveTask
    | NonPrimitiveTask
    deriving (Eq, Ord, Show)
instance Functor Primitiveness where
    fmap _ PrimitiveTask = PrimitiveTask
    fmap _ NonPrimitiveTask = NonPrimitiveTask
instance TypeAspect Primitiveness where
    baseType' = PrimitiveTask
    addType' t1 t2
        | t1 == t2 = t1
        | otherwise = NonPrimitiveTask
primitive :: forall f . (Primitiveness ::<:: f) => FullType f 
primitive = tinject PrimitiveTask
nonPrimitive :: forall f . (Primitiveness ::<:: f) => FullType f 
nonPrimitive = tinject NonPrimitiveTask

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
instance Functor LastPosition where
    fmap _ NoPosition = NoPosition
    fmap _ NotLastPosition = NotLastPosition
    fmap _ LastPosition = LastPosition
    fmap _ BothPosition = BothPosition
instance TypeAspect LastPosition where
    baseType' = NoPosition
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
bothPosition :: forall f . (LastPosition ::<:: f) => FullType f
bothPosition = tinject BothPosition

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
instance Functor ParentPosition where
    fmap _ PNoPosition = PNoPosition
    fmap _ PNotLastPosition = PNotLastPosition
    fmap _ PLastPosition = PLastPosition
    fmap _ PBothPosition = PBothPosition
instance TypeAspect ParentPosition where
    baseType' = PNoPosition
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
        subt = if extendsType taskt bothPosition then pBothPosition
            else if extendsType taskt isLast then pIsLast
            else if extendsType taskt notLast then pNotLast
            else pNoPosition
        subtasks = enumerateTasks m
    in
    foldl (\tm (_, task) -> updateMap (taskName task) subt tm) initTM subtasks
    
parentPositionTyper :: (HasTaskLists m,
    HasTaskHead StdTaskHead m,
    HasActions m d) =>
    Typer d
parentPositionTyper = applyMTyper parentPositionMTyper
