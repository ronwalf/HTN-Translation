{-# LANGUAGE
    FlexibleContexts
    #-}
module HTNTranslation.ProgressionBounds (
  boundProgression
) where

import HTNTranslation.HTNPDDL

-- |For mostly tail recursive HTN domains, 'boundProgression' calculates an
-- upper bound on the size of the task network reachable via progress.
boundProgression :: 
    ( Monad m
    , HasTaskHead (Maybe (Expr PDDLAtom)) action
    , HasTaskLists action
    , HasTaskConstraints action
    ) => domain -> problem -> m Int
boundProgression domain problem = undefined
