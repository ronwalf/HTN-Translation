module HTNTranslation.Translation

where

import Data.List
import Data.Maybe

import HTNTranslation.HTNPDDL
import Planning.PDDL.Repair

collectTasks (Task name args) = [(name, args)]
collectTasks (OrderedTasks tl) = concatMap collectTasks tl

translateAction (controlPred, params) action = action

genericArgNames = [ "x" ++ show n | n <- [1..] ]
mkGenericArgs args = zip (take (length args) genericArgNames) (repeat Nothing)

mkParameters terms =
    mapMaybe getVarName terms
    where
        getVarName (Var n) = Just (n, Nothing)
        getVarName _ = Nothing

start = ("start_" ++)
working = ("working_" ++)


startPred (name, args) = Atomic (start name) args
workingPred (name, args) = Atomic (working name) args


finishEffects (name, args) = [
    Not $ Atomic (start name) args, 
    Not $ Atomic (working name) args]

--translateMethod :: Method -> [DomainItem]
--translateMethod method =

--translateBranch :: (String, [Term]) -> Condition -> [DomainItem]
--translateBranch task precond =

translateMethod method =
    let
        task = taskHead method
        preconds = inits $ map bprecondition $ branches method
        --preconds = repeat []
        prefix = fst task ++ "_" ++ methodName method
    in
    concatMap (\ (pl, br) ->  translateBranch task prefix pl br) $ 
        zip preconds $ branches method

translateBranch task prefix precondList branch =
    let
        bprefix = prefix ++ "_" ++ (branchName branch)
        preconds = bprecondition branch : map Not precondList
        branchTasks = collectTasks $ tasks branch
        actionList = translateTL task bprefix branchTasks
        firstTask = let action = (head actionList) in 
            action {
                precondition = And preconds
            }
    in
    firstTask : tail actionList

translateTL task prefix [] = 
    let
        action = emptyAction {
            actionName = prefix ++ "_empty",
            parameters = mkParameters $ snd task,
            precondition = And [startPred task, Not $ workingPred task],
            effect = Not $ startPred task
        }
    in
    [action]

translateTL task prefix tl =
    let
        params = snd task
        names = [ prefix ++ "_" ++ show n | n <- [1..] ]
        lastTask = last tl
        rest = if (fst task == fst lastTask) then
                take (length tl - 1) tl
            else
                tl
        lastAction = if (fst task == fst lastTask) then
                let name = prefix ++ "_recurse" in emptyAction { 
                    actionName = name,
                    parameters = mkParameters params,
                    effect = And [
                        Not $ startPred task,
                        Not $ workingPred task,
                        startPred lastTask
                    ]
                }
            else
                emptyAction {
                    actionName = prefix ++ "_last",
                    parameters = mkParameters params,
                    effect = And [
                        Not $ startPred task,
                        Not $ workingPred task
                    ]
                }
        actions = foldl (createPredecessor params) [lastAction] $ zip names rest
    in
    actions
    where
        createPredecessor params (h:r) (name, task) =
            let
                action = emptyAction {
                    actionName = name,
                    parameters = mkParameters params,
                    effect = And [startPred task, startPred (actionName h, params)]
                }
                nh = h {
                    precondition = And [ Not $ startPred task, Not $ workingPred task ]
                }
            in
            action : nh : r
        



translate :: (Monad a) => HTNDomain -> a Domain
translate (HTNDomain (info, methods, items)) =
    let
        allBranches = concatMap branches methods
        allTasks = concatMap collectTasks (map tasks allBranches)
        tasknames = nub $ map fst $ allTasks
        (mentionedItems, unmentioned) = partition (nameFilter tasknames) items
        controlledItems = map addControl mentionedItems 
        translated = concatMap translateMethod methods
    in
    repairDomain $ 
    Domain (info, translated ++ controlledItems ++ unmentioned)
    where
        nameFilter mentioned (Action { actionName = aname }) = elem aname mentioned
        --nameFilter _ _ = False
        addControl action@(Action { actionName = name, precondition = cond }) =
            let
                pred = Atomic ("start_" ++ name) $ map (Var . fst) $ parameters action
            in
            action { precondition = And [ pred, cond ],
                     effect = And [ Not pred, effect action ]
            }

