{-# LANGUAGE
    FlexibleContexts,
    ScopedTypeVariables
  #-}
module HTNTranslation.ProgressionMin (minProgression) where

import Data.Function (on)
import Data.List
import Data.Maybe (fromMaybe)
--import Debug.Trace

import HTNTranslation.HTNPDDL
import HTNTranslation.ProgressionBounds (findMethods, findReachableTasks)

-- |Provides the minimum progression bound necessary for
minProgression :: 
    ( HasName action
    , HasTaskHead (Maybe (Expr PDDLAtom)) action
    , HasTaskLists TermExpr action
    , HasTaskConstraints action
    , HasActions action domain
    , HasName problem
    , HasTaskLists ConstTermExpr problem
    , HasTaskConstraints problem
    ) => domain -> problem -> (Int, [(String, Int)])
minProgression domain problem =
    (boundsGame bounds problem, bounds)
    where
    bounds = map fst $ snd $
        until (not . fst)
            (iterateBounds domain) $ 
        (\lb -> (True, lb)) $ 
        map (\t -> ((t, maxBound), findMethods domain t)) $
        findReachableTasks domain problem
    
iterateBounds :: forall action a domain . 
    ( HasName action
    , HasTaskHead (Maybe (Expr PDDLAtom)) action
    , HasTaskLists a action
    , HasTaskConstraints action
    , HasActions action domain
    ) => domain -> (Bool, [((String, Int), [action])]) -> (Bool, [((String, Int), [action])])
iterateBounds domain (_, cbounds) = {-# SCC "iterateBounds" #-}
    foldl (\(changed, l) ((t,tprevb),actions) ->
            let tnextb = foldl reboundAction tprevb $ filter (isPlausible tprevb) actions in 
            (changed || tnextb < tprevb, ((t, tnextb), actions) : l))
        (False, []) cbounds
    where
    bounds :: [(String, Int)]
    bounds = map fst cbounds
    reboundAction :: Int -> action -> Int
    reboundAction bound action =
        min bound $ boundsGame bounds action
    -- only consider rebounding with plausible methods (subtasks all have lower bounds)
    isPlausible :: Int -> action -> Bool
    isPlausible bound method = and $
        map ((< bound) . taskBound bounds . taskName . snd) $
        enumerateTasks method

{-
iterateBounds :: forall action a domain . 
    ( HasName action
    , HasTaskHead (Maybe (Expr PDDLAtom)) action
    , HasTaskLists a action
    , HasTaskConstraints action
    , HasActions action domain
    ) => domain -> [(Bool, (String, Int))]-> [(Bool, (String, Int))]
iterateBounds domain cbounds = {-# SCC "iterateBounds" #-}
    foldl (\l tb -> reboundTask tb : l) [] cbounds
    where
    bounds :: [(String, Int)]
    bounds = map snd cbounds
    reboundTask :: (Bool, (String, Int)) -> (Bool, (String, Int))
    reboundTask (_, (task, bound)) = (\(changed, bound') -> (changed, (task, bound'))) $
        foldl reboundAction (False, bound) $
        filter (isPlausible bound) $
        findMethods domain task
    reboundAction :: (Bool, Int) -> action -> (Bool, Int)
    reboundAction (changed, bound) action =
        let bound' = boundsGame bounds action in
        (changed || bound' < bound, min bound bound')
    -- only consider rebounding with plausible methods (subtasks all have lower bounds)
    isPlausible :: Int -> action -> Bool
    isPlausible bound method = and $
        map ((< bound) . taskBound bounds . taskName . snd) $
        enumerateTasks method
-}

boundsGame :: 
    ( HasName action
    , HasTaskLists a action
    , HasTaskConstraints action
    ) => [(String, Int)] -> action -> Int
boundsGame bounds method = 
    {-# SCC "bounds-game" #-} minimum $ bg (max 1 $ length tasks) taskWeights
    where
    tasks :: [(Int, String)]
    tasks = map (\(n, t) -> (n, taskName t)) (enumerateTasks method)
    taskConstraints :: [(Int, [Int])]
    taskConstraints = map (\(t,_) -> (t, map fst $ findPrevTasks method t)) tasks
    taskWeights :: [(Int, Int)]
    taskWeights = map (\(n,t) -> (n, taskBound bounds t)) tasks

    -- The actual game itself
    bg :: Int -> [(Int, Int)] -> [Int]
    bg cmin [] = [cmin]
    bg cmin tn
        -- Current bound is bigger than the largest min bound plus the size of the remaining list
        | cmin >= (maximum $ map snd tn) + length tn - 1 = [cmin]
        -- Special case for completely unordered tasks
        -- | length tn == length tnc = [maximum $ map lweight $ tails $ sort $ map snd tn]
        | otherwise = (\(ccmin, ctn) -> bg (max cmin ccmin) ctn) $ tnc
        where
        tnc :: (Int, [(Int, Int)])
        tnc = progressTask tn $ minimumBy (compare `on` snd) $ unconstrained tn
    progressTask :: [(Int, Int)] -> (Int, Int) -> (Int, [(Int, Int)])
    progressTask tn t@(_,lw) = 
        (lw + length tn', tn')
        where
        tn' = delete t tn
    -- Returns all tasks that do not have parents in current task network
    unconstrained :: [(Int, Int)] -> [(Int, Int)]
    unconstrained tn = 
        filter ( null . intersect labels . fromMaybe [] . flip lookup taskConstraints . fst) tn
        where
        labels :: [Int]
        labels = map fst tn


taskBound :: [(String, Int)] -> String -> Int
taskBound bounds = maybe maxBound id . flip lookup bounds

