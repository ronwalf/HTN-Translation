module HTNTranslation.Translation

where

import HTNTranslation.HTNPDDL

collectTasks (Task name args) = [(name, args)]
collectTasks (OrderedTasks tl) = concatMap collectTasks tl

translateAction (controlPred, params) action = action

genericArgNames = [ "x" ++ show n | n <- [1..] ]
mkGenericArgs args = zip (take (length args) genericArgNames) (repeat Nothing)

startPred (name, args) = ("start_" ++ name, mkGenericArgs args)
workingPred (name, args) = ("working_" ++ name, mkGenericArgs args)



translate :: HTNDomain -> Domain
translate (HTNDomain (info, methods, items)) =
    let
        tasknames = map fst $ concatMap collectTasks (map tasks methods)
        mentionedItems = filter (nameFilter tasknames) items
        
    in
    Domain (info, mentionedItems)
    where
        nameFilter mentioned (Action { actionName = aname }) = elem aname mentioned
        nameFilter _ _ = False

