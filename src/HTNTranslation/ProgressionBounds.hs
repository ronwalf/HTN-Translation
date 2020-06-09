{-# OPTIONS_GHC
    -freduction-depth=30
  #-}
{-# LANGUAGE
    DeriveDataTypeable,
    FlexibleContexts,
    FlexibleInstances,
    FunctionalDependencies,
    MultiParamTypeClasses,
    RankNTypes,
    ScopedTypeVariables,
    StandaloneDeriving,
    TypeOperators,
    TypeSynonymInstances
  #-}
module HTNTranslation.ProgressionBounds (
    boundProgression,
    findMethods,
    listTaskNames,
    findReachableTasks,
    findReachableTasks'
) where

import Control.Monad (liftM, when)
import Data.Graph
import Data.List
import Data.Maybe
import Data.Text (Text)

import HTNTranslation.HTNPDDL

-- |For mostly tail recursive HTN domains, 'boundProgression' calculates an
-- upper bound on the size of the task network reachable via progression.
-- Returns the problem bound and a list of bounds for sets of
-- multually-recursive tasks in topological order.
boundProgression :: forall action domain problem m .
    ( Monad m
    , HasName action
    , HasTaskHead (Maybe (Expr PDDLAtom)) action
    , HasTaskLists TermExpr action
    , HasTaskConstraints action
    , HasActions action domain
    , HasName problem
    , HasTaskLists ConstTermExpr problem
    , HasTaskConstraints problem
    ) => domain -> problem -> m (Int, [([Text], Int)])
boundProgression domain problem = do
    noHeadlessMethodsCheck
    mapM_ tailRecursionCheck taskCycles
    let bounds = progressionBound taskCycles
    return (boundsGame problem bounds, bounds)
    where
    noHeadlessMethodsCheck :: m ()
    noHeadlessMethodsCheck = flip mapM_ (getActions domain) $ \a ->
        when ((isNothing $ getTaskHead a) && (not $ null $ getTaskLists a)) $
        error $ (show $ getName a) ++ " has subtasks but no task head."
    tailRecursionCheck :: [Text] -> m ()
    tailRecursionCheck cyclic =
        flip mapM_ cyclic $ \t ->
        mapM_ (methodRecursionCheck cyclic) $
        findMethods domain t
    methodRecursionCheck :: [Text] -> action -> m ()
    methodRecursionCheck cyclic action =
        flip mapM_
            (filter (not . (== findLastTask action) . Just) $
            enumerateTasks action) $
        \(_, ts) -> when (taskName ts `elem` cyclic) $ error $ show (getName action) ++ " has non-tail recursion through task " ++ (show $ taskName ts) ++ " in problem " ++ (show $ getName problem)
    -- |Bound progression for a list of task cycles
    progressionBound :: [[Text]] -> [([Text], Int)]
    progressionBound = foldr boundSet []
        where
        boundSet :: [Text] -> [([Text], Int)] -> [([Text], Int)]
        boundSet tasks bounds =
            (\bound -> (tasks, bound) : bounds) $
            maximum $ (1:) $
            map (flip boundsGame bounds) $
            concatMap (findMethods domain) tasks
    -- |Find all task cycles in a domain (topological ordering)
    taskCycles :: [[Text]]
    taskCycles =
        reverse $
        map flattenSCC $
        stronglyConnComp $
        map gatherTaskEdges $
        findReachableTasks domain $
        listTaskNames problem
        where
        gatherTaskEdges :: Text -> (Text, Text, [Text])
        gatherTaskEdges task = (task, task,
            nub $ sort $
            concatMap (map taskName . snd) $
            concatMap getTaskLists $
            findMethods domain task)


-- |Play the bounds game for a method, returning the max bound given already known bounds.
-- Assumes unknown bounds are '1', since they should reflect tasks that are mutually recursive
-- with this method's task head.
boundsGame :: forall a action .
    ( HasName action
    , HasTaskLists a action
    , HasTaskConstraints action
    ) => action -> [([Text], Int)] -> Int
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
    tasks :: [(Int, Text)]
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
            = maybe (error $ "Non-last task " ++ show (fromJust (lookup t tasks)) ++ " has unknown bound in " ++ show (getName action) ++ " (known bounds: " ++ show bounds ++ ")") id $
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



-- |Find all reachable tasks for a given domain-problem pair
findReachableTasks ::
    ( HasTaskHead (Maybe (Expr PDDLAtom)) action
    , HasTaskLists a action
    , HasActions action domain
    ) => domain -> [Text] -> [Text]
findReachableTasks domain =
    findReachableTasks' (concatMap listTaskNames . findMethods domain)

findReachableTasks' :: (Text -> [Text]) -> [Text] -> [Text]
findReachableTasks' mlookup =
    foldl' frt [] 
    where
    frt :: [Text] -> Text -> [Text]
    frt explored task
         | task `elem` explored = explored
         | otherwise = foldl' frt (task : explored) $
                mlookup task


-- |Find all methods for a given task name (should probably refactor into utils)
findMethods :: forall action domain .
    ( HasTaskHead (Maybe (Expr PDDLAtom)) action
    , HasActions action domain
    ) => domain -> Text -> [action]
findMethods domain task = {-# SCC "findMethods" #-}
    filter (fromMaybe False . fmap ((== task) . taskName) . getTaskHead) $
    getActions domain
