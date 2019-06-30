{-# OPTIONS_GHC
    -freduction-depth=30
    -Wall
  #-}
{-# LANGUAGE
    FlexibleContexts,
    FlexibleInstances,
    OverloadedStrings,
    RankNTypes,
    ScopedTypeVariables,
    TypeOperators,
    UndecidableInstances
  #-}
module Main where

import Control.Monad (liftM)
import Data.Function (on)
import Data.List
import Data.Maybe
import Data.Text (Text, append, pack)
import qualified Data.Text as DT
--import Debug.Trace
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import Text.ParserCombinators.Parsec hiding (space)
import qualified Text.ParserCombinators.Parsec.Token as T

import Planning.PDDL.PDDL3_0
import Planning.PDDL.Parser
import Planning.Util (cfConversion, findAtoms, findFreeVars, FreeVarsFindable, liftE)

import HTNTranslation.HTNPDDL
import HTNTranslation.ProgressionBounds (findReachableTasks, findMethods)

nunidString :: Text
nunidString = "nunlift_id"
nunid :: (Atomic t :<: f) => t -> t -> Expr f
nunid x y = eAtomic nunidString [x,y]
nuntmemPrefix :: Text 
nuntmemPrefix = "nunlift_mem_of_"
nuntmem :: (Atomic t :<: f) => t -> Text -> Expr f
nuntmem x t = eAtomic (append nuntmemPrefix t) [x]

class Functor f => SpecialPredsFindable f where
    findNunPreds' :: f (Bool, [Text]) -> (Bool, [Text])
findNunPreds :: SpecialPredsFindable f => Expr f -> (Bool, [Text])
findNunPreds e =
    let (i, t) = foldExpr findNunPreds' e in
    (i, nub $ sort $ t)

findAllSpecials :: StandardHTNDomain -> (Bool, [Text])
findAllSpecials dom =
    foldr (\(xi, xt) (yi, yt) -> (xi || yi, xt ++ yt)) (False, []) $
    map (findNunPreds . snd) $
    concatMap getPrecondition $
    getActions dom

instance (SpecialPredsFindable f, SpecialPredsFindable g) => SpecialPredsFindable (f :+: g) where
    findNunPreds' (Inl x) = findNunPreds' x
    findNunPreds' (Inr y) = findNunPreds' y

instance SpecialPredsFindable (Atomic (Expr t)) where
    findNunPreds' (Atomic p _) = case DT.stripPrefix nuntmemPrefix p of
            Just t -> (False, [t])
            Nothing -> if (p == nunidString) then (True, []) else (False, [])

instance SpecialPredsFindable Not where
    findNunPreds' (Not t) = t

instance SpecialPredsFindable (ForAll vt) where
    findNunPreds' (ForAll _ t) = t

instance SpecialPredsFindable (Exists vt) where
    findNunPreds' (Exists _ t) = t

instance SpecialPredsFindable Imply where
    findNunPreds' (Imply (xi, xt) (yi, yt)) = (xi || yi, xt ++ yt)

instance SpecialPredsFindable And where
    findNunPreds' (And el) = foldr (\(xi, xt) (yi, yt) -> (xi || yi, xt ++ yt)) (False, []) el

instance SpecialPredsFindable Or where
    findNunPreds' (Or el) = foldr (\(xi, xt) (yi, yt) -> (xi || yi, xt ++ yt)) (False, []) el


class (Functor f, FreeVarsFindable f) => TypedVarsFindable f where
    findTypedVars' :: [TypedPredicateExpr] -> f [TypedVarExpr] -> [TypedVarExpr]
findTypedVars :: TypedVarsFindable f => [TypedPredicateExpr] -> Expr f -> [TypedVarExpr]
findTypedVars typeDefs = foldExpr (findTypedVars' typeDefs)

instance (TypedVarsFindable f, TypedVarsFindable g) => TypedVarsFindable (f :+: g) where
    findTypedVars' typeDefs (Inl x) = findTypedVars' typeDefs x
    findTypedVars' typeDefs (Inr y) = findTypedVars' typeDefs y

instance FreeVarsFindable t => TypedVarsFindable (Atomic (Expr t)) where
    findTypedVars' typeDefs (Atomic p tl) = concat $ zipWith pvlookup [0..] tl
        where
        pvlookup :: Int -> (Expr t) -> [TypedVarExpr]
        pvlookup n t =
            let types :: [Text] =
                    concatMap (getType . (!! n)) $
                    filter ((> n) . length) $
                    map taskArgs $
                    filter ((==) p . taskName) typeDefs in
            map (flip eTyped types) $ findFreeVars t -- Actually quite wrong if functions are allowed.


findAllTypedVars :: TypedVarsFindable f => [TypedPredicateExpr] -> [Expr f] -> [TypedVarExpr]
findAllTypedVars typeDefs = concatMap retype . groupBy ((==) `on` removeType) . sort . concatMap (findTypedVars typeDefs)
    where
    retype :: [TypedVarExpr] -> [TypedVarExpr]
    retype [] = []
    retype tl@(t:_) = [eTyped (removeType t) $ nub $ sort $ concatMap getType tl]



type LiftedHTNProblem = HProblem InitLiteralExpr PreferenceGDExpr ConstraintGDExpr TermExpr


liftedProblemParser :: GenParser Char LiftedHTNProblem LiftedHTNProblem
liftedProblemParser =
    let
        stateP = T.parens pddlExprLexer $ initLiteralParser pddlExprLexer :: CharParser LiftedHTNProblem InitLiteralExpr
        goalP = prefGDParser pddlExprLexer :: CharParser LiftedHTNProblem PreferenceGDExpr
        constraintP = constraintGDParser pddlExprLexer :: CharParser LiftedHTNProblem ConstraintGDExpr
        infoP =
            (taskListParser htnDescLexer (termParser pddlExprLexer) >>= updateState)
            <|> (taskConstraintParser htnDescLexer >>= updateState)
            <|> problemInfoParser htnDescLexer stateP goalP constraintP
    in
    problemParser htnDescLexer infoP


parseLiftedHTN :: SourceName -> String -> Either ParseError LiftedHTNProblem
parseLiftedHTN source input =
    runParser liftedProblemParser emptyHProblem source input


-- Copy a problem without its initial task list and constraints
copyProblem :: LiftedHTNProblem -> StandardHTNProblem
copyProblem prob =
    setName (getName prob) $
    setDomainName (getDomainName prob) $
    setRequirements (getRequirements prob) $
    setConstants (getConstants prob) $
    setInitial (getInitial prob) $
    setGoal (getGoal prob) $
    setConstraints (getConstraints prob) $
    (emptyHProblem :: StandardHTNProblem)


trivialConversion :: (Monad m) => LiftedHTNProblem -> m StandardHTNProblem
trivialConversion prob = do
    taskLists <- mapM convertList $ getTaskLists prob
    return $
        setTaskLists taskLists $
        setTaskConstraints (getTaskConstraints prob) $
        copyProblem prob
    where
    convertList :: (Monad m) => (Maybe Text, [Expr (Atomic TermExpr)]) -> m (Maybe Text, [Expr (Atomic ConstTermExpr)])
    convertList (name, tasks) = do
        ctasks <- mapM (\t -> do {(tl :: [ConstTermExpr]) <- mapM cfConversion (taskArgs t); return (eAtomic (taskName t) tl)}) tasks
        return (name, ctasks :: [Expr (Atomic ConstTermExpr)])


injectionConversion :: StandardHTNDomain -> LiftedHTNProblem -> (StandardHTNDomain, StandardHTNProblem)
injectionConversion dom lprob =
    ( injectDomain $ injectMethod (getTaskLists lprob) (getTaskConstraints lprob)
    , setTaskLists [(Nothing, [tname :: Expr (Atomic ConstTermExpr)])] $ copyProblem lprob)
    where
    tname :: forall t . Expr (Atomic t)
    tname = eAtomic "htn_initial_task" ([] :: [t])
    injectDomain :: StandardMethod -> StandardHTNDomain
    injectDomain m =
        setTaskHead (tname : getTaskHead dom) $
        setActions (m : getActions dom) $
        dom
    injectMethod :: [TaskList TermExpr] -> [TaskConstraint] -> StandardMethod
    injectMethod taskLists taskConstraints =
        let
            tdefs =
                -- (\x -> trace (("TDefs: "++) $ show $ map pddlDoc x) x) $
                getTaskHead dom ++ getPredicates dom
            params =
                -- (\x -> trace (("Params: " ++) $ show $ pddlDoc x) x) $
                findAllTypedVars tdefs $
                -- (\x -> trace (("Tasks: " ++) $ show $ map pddlDoc x) x) $
                concatMap snd taskLists
        in
        setName "assert_initial_tasks" $
        setTaskHead (Just tname) $
        setParameters params $
        setTaskLists taskLists $
        setTaskConstraints taskConstraints $
        defaultMethod

removeEmptyTypes :: StandardHTNDomain -> StandardHTNProblem -> StandardHTNDomain
removeEmptyTypes dom prob =
    let
        usedBaseTypes :: [Text] = concatMap getType $ getConstants dom ++ getConstants prob
        usedTypes :: [Text] = nub $ sort $ fst $ last $
                takeWhile (not . null . snd) $
                iterate (\(at, _) ->
                    let nt = filter (flip notElem at) $
                             concatMap getType $
                             filter (flip elem at . removeType) $
                             getTypes dom
                    in
                    (at ++ nt, nt))
                (usedBaseTypes, usedBaseTypes)
        missingTPNames :: [Text] =
                map taskName $
                filter (or . map (and . (\x -> (not $ null x) : x) . map (flip notElem usedTypes) . getType) . taskArgs) $
                (getTaskHead dom ++ getPredicates dom)
        useableActions :: [StandardMethod] =
                -- Doesn't remove old types from params with (either...)
                filter (not . missingSubtask) $
                filter (not . missingParameter) $
                getActions dom
        missingParameter :: StandardMethod -> Bool
        missingParameter = or . map (and . (\x -> (not $ null x) : x) . map (flip notElem usedTypes) . getType) . getParameters
        missingSubtask :: StandardMethod -> Bool
        missingSubtask = or . map (flip elem missingTPNames . taskName . snd) . enumerateTasks
    in
    setTypes (filter (flip elem usedTypes . removeType) $ getTypes dom) $
    setTaskHead (filter (flip notElem missingTPNames . taskName) $ getTaskHead dom) $
    setPredicates (filter (flip notElem missingTPNames . taskName) $ getPredicates dom) $
    setActions useableActions $
    dom


compileTypes :: (StandardHTNDomain, StandardHTNProblem) -> (StandardHTNDomain, StandardHTNProblem)
compileTypes (dom, prob) =
    ( setTypes [eTyped ("POBJ" :: Text) []]
      $ setTaskHead utTasks
      $ setPredicates utPreds
      $ setConstants utDConsts
      $ setActions (map utAction $ getActions dom) dom
    , setConstants utConsts $ setInitial utInitial prob )
    where
    allTypes :: [Text]
    allTypes = nub $ sort $ concatMap (\t -> removeType t : getType t) $ getTypes dom
    retype :: forall f g . Untypeable f g => Expr f -> Expr (Typed g)
    retype tvar = eTyped (removeType tvar) ["POBJ"]
    utDConsts :: [TypedConstExpr]
    utDConsts = map retype $ getConstants dom
    utConsts :: [TypedConstExpr]
    utConsts = map retype $ getConstants prob
    utTasks :: [TypedPredicateExpr]
    utTasks = map (\l -> eAtomic (taskName l) (map retype (taskArgs l) :: [TypedVarExpr])) $ getTaskHead dom
    utPreds :: [TypedPredicateExpr]
    utPreds =
        map (flip eAtomic [eTyped (eVar "x" :: Expr Var) ["POBJ"] :: TypedVarExpr]) allTypes
        ++ (map (\l -> eAtomic (taskName l) (map retype (taskArgs l) :: [TypedVarExpr])) $ getPredicates dom)
    utInitial :: [InitLiteralExpr]
    utInitial =
        (flip concatMap allTypes $ \t ->
            map (eAtomic t . (:[]) . (liftE :: Expr Const -> ConstTermExpr) . removeType) $ nub $ sort $ findmems t)
        ++ getInitial prob
    -- Currently ignores all quantification
    utAction :: StandardMethod -> StandardMethod
    utAction action =
        setParameters (map retype $ getParameters action) $
        setPrecondition (concat (map paramPreds (getParameters action) ++ [getPrecondition action])) $
        action
    paramPreds :: TypedVarExpr -> [(Maybe Text, GDExpr)]
    paramPreds (In (Typed _ [])) = []
    paramPreds (In (Typed v [t])) = [(Nothing, eAtomic t [liftE v :: TermExpr])]
    paramPreds (In (Typed v tl)) = [(Nothing, eOr $ map (flip eAtomic [liftE v :: TermExpr]) tl)]

    findmems t =
        filter (\c -> t `elem` getType c) (getConstants dom ++ getConstants prob)
        ++ concatMap (findmems . removeType)
                     (filter (elem t . getType) $ getTypes dom)




processSpecials :: (StandardHTNDomain, StandardHTNProblem) -> (StandardHTNDomain, StandardHTNProblem)
processSpecials (dom, prob) =
    let (usesId, tmems) = findAllSpecials dom in
    addIdentity usesId $
    flip (foldl addTypeMem) tmems $
    (dom, prob)
    where
    addIdentity :: Bool -> (StandardHTNDomain, StandardHTNProblem) -> (StandardHTNDomain, StandardHTNProblem)
    addIdentity False (dom', prob') = (dom', prob')
    addIdentity True (dom', prob') =
        ( setPredicates (nunid (eTyped (eVar "x" :: Expr Var) [] :: TypedVarExpr) (eTyped (eVar "y" :: Expr Var) []) : getPredicates dom') dom'
        , setInitial ([nunid (liftE (removeType i) :: ConstTermExpr) (liftE $ removeType i)
                      | i <- getConstants dom' ++ getConstants prob']
                      ++ getInitial prob')
                     prob')
    addTypeMem :: (StandardHTNDomain, StandardHTNProblem) -> Text -> (StandardHTNDomain, StandardHTNProblem)
    addTypeMem (dom', prob') t =
        ( setPredicates (nuntmem (eTyped (eVar "x" :: Expr Var) [] :: TypedVarExpr) t : getPredicates dom') dom'
        , setInitial
            ( map (flip nuntmem t :: ConstTermExpr -> InitLiteralExpr)
                  (nub $ sort $ map (liftE . removeType) $ findmems t)
            ++ getInitial prob') prob')
    findmems t =
        filter (\c -> t `elem` getType c) (getConstants dom ++ getConstants prob)
        ++ concatMap (findmems . removeType)
                     (filter (elem t . getType) $ getTypes dom)


stripUnreachable :: StandardHTNDomain -> StandardHTNProblem -> StandardHTNDomain
stripUnreachable dom prob =
    setActions (reachable ++ headless) dom
    where
    headless = filter (isNothing . getTaskHead) $ getActions dom
    reachable = 
        concatMap (findMethods dom :: Text -> [StandardMethod]) $
        findReachableTasks dom $
        listTaskNames prob


ensurePrimitiveTasks :: StandardHTNDomain -> StandardHTNDomain
ensurePrimitiveTasks dom =
    flip setTaskHead dom $
    (++ getTaskHead dom) $
    map (\a -> eAtomic (getName a) (getParameters a)) $
    filter (\a -> (getName a) `notElem` (map taskName $ getTaskHead dom)) $
    filter (\a -> (Just $ getName a) == ((getTaskHead a) >>= (return . taskName))) $
    getActions dom

ensurePredicates :: StandardHTNDomain -> StandardHTNProblem -> StandardHTNDomain
ensurePredicates dom prob = setPredicates (getPredicates dom ++ tpreds) dom
    where
    typedVars :: [TypedVarExpr]
    typedVars = map (flip eTyped ["POBJ"] . (eVar :: Text -> Expr Var) . (append "v") . pack . show) ([1..] :: [Int])
    definedAtoms :: [Text]
    definedAtoms = ("=":) $ map taskName $ getPredicates dom
    nameArity :: forall g . Expr (Atomic g) -> (Text, Int)
    nameArity p = (taskName p, length $ taskArgs p)
    precondAtoms :: PDDLPrecond -> [(Text, Int)]
    precondAtoms = map nameArity . (findAtoms :: GDExpr -> [Expr (Atomic TermExpr)]) . snd
    effectAtoms :: PDDLEffect -> [(Text,Int)]
    effectAtoms (_, Just p, e) = (map nameArity (findAtoms p :: [Expr (Atomic TermExpr)])) ++ (effectAtoms ([], Nothing, e))
    effectAtoms (_, Nothing, e) = map nameArity $ concatMap (findAtoms :: EffectDExpr -> [Expr (Atomic TermExpr)]) e
    actionAtoms :: StandardMethod -> [(Text, Int)]
    actionAtoms action =
        concatMap precondAtoms (getPrecondition action)
        ++ concatMap effectAtoms (getEffect action)
    tpreds :: [TypedPredicateExpr]
    tpreds = map (\(p,n) -> eAtomic p (take n typedVars)) $
        filter (flip notElem definedAtoms . fst) $
        nub $ sort $
        (concatMap actionAtoms (getActions dom))
        ++ (map nameArity $ concatMap (findAtoms :: InitLiteralExpr -> [Expr (Atomic ConstTermExpr)]) $ getInitial prob)

processProblem :: StandardHTNDomain -> String -> IO ()
processProblem domain probFile = do
    contents <- readFile probFile
    problem <- errCheck $ parseLiftedHTN probFile contents
    let (dom',prob') =
--            (\x -> trace (("Post type removal:\n" ++) $
--                show $ pddlDoc $ fst x) x) $
            compileTypes $ (\(d, p) -> (removeEmptyTypes d p, p)) $
--            (\x -> trace (("Pre type removal:\n" ++) $
--                show $ pddlDoc $ fst x) x) $
--            (\x -> trace (("Reachable: " ++ ) $
--                show $ findReachableTasks (fst x) (snd x)) x) $
            case (trivialConversion problem) of
                (Just p) -> (domain, p)
                Nothing -> injectionConversion domain problem
    saveFiles (flip ensurePredicates prob' $ stripUnreachable dom' prob') prob' probFile
    return ()

saveFiles :: StandardHTNDomain -> StandardHTNProblem -> String -> IO ()
saveFiles dom prob fname = do
    let oname = '-' : addExtension (takeBaseName fname) "hpddl"
    writeFile ('d' : oname) (show $ pddlDoc dom)
    writeFile ('p' : oname) (show $ pddlDoc prob)
    return ()

errCheck :: (Show t) => Either t b -> IO b
errCheck (Left err) = do
    hPrint stderr err
    exitFailure
errCheck (Right prob) = return prob


main :: IO ()
main = do
    argv <- getArgs
    (domFile, files) <- case argv of
        (dom : files @ (_ : _)) -> do return (dom, files)
        _ -> ioError $ userError "Usage: htnunlift domain.hpddl problem1.hpddl [problem2.hpddl, ...]\nOutputs: d-problem1.hpddl, p-problem1.hpddl, d-problem2.hpddl..."
    domContents <- readFile domFile
    domain <-  liftM ensurePrimitiveTasks (errCheck $ parseHTNPDDL domFile domContents)
    mapM_ (processProblem domain) files

    return ()
