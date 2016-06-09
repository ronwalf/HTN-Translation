{-# OPTIONS_GHC
    -fcontext-stack=30
  #-}
{-# LANGUAGE
    DeriveDataTypeable,
    FlexibleContexts,
    FlexibleInstances,
    FunctionalDependencies,
    MultiParamTypeClasses,
    OverlappingInstances, 
    RankNTypes,
    ScopedTypeVariables,
    StandaloneDeriving, 
    TypeOperators,
    TypeSynonymInstances
  #-}
module HTNTranslation.ProgressionBounds (boundProgression, findMethods, findReachableTasks) where

import Control.Monad (liftM, when)
import Data.Graph
import Data.List
import Data.Maybe

import HTNTranslation.HTNPDDL

-- |For mostly tail recursive HTN domains, 'boundProgression' calculates an
-- upper bound on the size of the task network reachable via progression.
-- Returns the problem bound and a list of bounds for sets of 
-- multually-recursive tasks in topological order.
boundProgression :: 
    ( Monad m
    , HasName action
    , HasTaskHead (Maybe (Expr PDDLAtom)) action
    , HasTaskLists TermExpr action
    , HasTaskConstraints action
    , HasActions action domain
    , HasName problem
    , HasTaskLists ConstTermExpr problem
    , HasTaskConstraints problem
    ) => domain -> problem -> m (Int, [([String], Int)])
boundProgression domain problem = do
    noHeadlessMethodsCheck
    let cycles = taskCycles domain problem
    mapM_ tailRecursionCheck cycles
    let bounds = progressionBound domain cycles 
    return (boundsGame problem bounds, bounds)
    where
    noHeadlessMethodsCheck = flip mapM_ (getActions domain) $ \a -> 
        when ((isNothing $ getTaskHead a) && (not $ null $ getTaskLists a)) $
        fail $ getName a ++ " has subtasks but no task head."
    tailRecursionCheck cyclic = 
        flip mapM_ cyclic $ \t ->
        mapM_ (methodRecursionCheck cyclic) $
        findMethods domain t
    methodRecursionCheck cyclic action = 
        flip mapM_ 
            (filter (not . (== findLastTask action) . Just) $ 
            enumerateTasks action) $ 
        \(_, ts) -> when (taskName ts `elem` cyclic) $ fail $ getName action ++ " has non-tail recursion through task " ++ (taskName ts) ++ " in problem " ++ (getName problem)


-- |Bound progression for a list of task cycles
progressionBound ::
    ( HasName action
    , HasTaskHead (Maybe (Expr PDDLAtom)) action
    , HasTaskLists a action
    , HasTaskConstraints action
    , HasActions action domain
    ) => domain -> [[String]] -> [([String], Int)]
progressionBound domain = foldr boundSet []
    where
    boundSet :: [String] -> [([String], Int)] -> [([String], Int)]
    boundSet tasks bounds = 
        (\bound -> (tasks, bound) : bounds) $
        maximum $ (1:) $ 
        map (flip boundsGame bounds) $
        concatMap (findMethods domain) tasks

-- |Play the bounds game for a method, returning the max bound given already known bounds.
-- Assumes unknown bounds are '1', since they should reflect tasks that are mutually recursive
-- with this method's task head.
boundsGame :: forall a action .
    ( HasName action
    , HasTaskLists a action
    , HasTaskConstraints action
    ) => action -> [([String], Int)] -> Int
boundsGame action bounds =
    maximum $ 
    (1:) $ 
    bg $ 
    reweightAll $ 
    zip (map fst tasks) $ 
    repeat unexpanded
    where
    unexpanded :: Int
    unexpanded = (-1)
    tasks :: [(Int, String)]
    tasks = map (\(n, t) -> (n, taskName t)) (enumerateTasks action)
    taskWeights :: [(Int, Int)]
    taskWeights = flip mapMaybe tasks $ \(t, ts) ->
        liftM (\(_,w) -> (t, w)) $
        find (elem ts . fst) bounds
    weight :: [(Int, Int)] -> Int
    weight = sum . map (max 1 . snd) -- Assume a weight of 1 for unexpanded tasks
    reweight :: [(Int, Int)] -> Int -> Int --
    reweight [_] t = maybe 1 id $ lookup t taskWeights 
    reweight tn t
        | null $ intersect (map fst tn) (map fst $ findPrevTasks action t)
            = maybe (error $ "Non-last task " ++ fromJust (lookup t tasks) ++ " has unknown bound in " ++ getName action ++ " (known bounds: " ++ show bounds ++ ")") id $
              lookup t taskWeights
        | otherwise = unexpanded
    reweightAll :: [(Int, Int)] -> [(Int, Int)]
    reweightAll tn = map (\t -> (t, reweight tn t)) $ map fst tn
    expand :: [(Int, Int)] -> Int -> Maybe [(Int, Int)]
    expand tn t = 
        let 
            parents = map fst $ findPrevTasks action t
            parentWeights = mapMaybe (flip lookup tn) parents
        in
        if (null parentWeights || unexpanded `elem` parentWeights)
            then Nothing
            else Just $ reweightAll $ flip filter tn $ not . flip elem parents . fst
    children :: [(Int, Int)] -> [[(Int, Int)]]
    children tn = mapMaybe (expand tn . fst) tn 
    bg :: [(Int, Int)] -> [Int]
    bg tn = weight tn : concatMap bg (children tn)

-- |Find all task cycles in a domain (topological ordering)
taskCycles :: forall a action domain problem .
    ( HasTaskHead (Maybe (Expr PDDLAtom)) action
    , HasTaskLists a action
    , HasTaskConstraints action
    , HasActions action domain
    , HasTaskLists ConstTermExpr problem
    ) => domain -> problem -> [[String]]
taskCycles domain problem =
    reverse $
    map flattenSCC $ 
    stronglyConnComp $
    map gatherTaskEdges $
    findReachableTasks domain problem
    where
    gatherTaskEdges :: String -> (String, String, [String])
    gatherTaskEdges task = (task, task,
        nub $ sort $
        concatMap (map taskName . snd) $
        concatMap getTaskLists $
        findMethods domain task)

    

-- |Find all reachable tasks for a given domain-problem pair
findReachableTasks :: forall a action domain problem . 
    ( HasTaskHead (Maybe (Expr PDDLAtom)) action
    , HasTaskLists a action
    , HasTaskConstraints action
    , HasActions action domain
    , HasTaskLists ConstTermExpr problem
    ) => domain -> problem -> [String]
findReachableTasks domain problem =
    foldl' frt [] $
    map (taskName . snd) $ 
    enumerateTasks problem
    where
    frt :: [String] -> String -> [String]
    frt explored task
         | task `elem` explored = explored
         | otherwise = foldl' frt (task : explored) $
                concatMap (map taskName . snd) $
                concatMap getTaskLists $
                findMethods domain task

-- |Find all methods for a given task name (should probably refactor into utils)
findMethods :: forall a action domain . 
    ( HasTaskHead (Maybe (Expr PDDLAtom)) action
    , HasTaskLists a action
    , HasTaskConstraints action
    , HasActions action domain
    ) => domain -> String -> [action]
findMethods domain task = {-# SCC "findMethods" #-}
    filter (fromMaybe False . fmap ((== task) . taskName) . getTaskHead) $
    getActions domain
