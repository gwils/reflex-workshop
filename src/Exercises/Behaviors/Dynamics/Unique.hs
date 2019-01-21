{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Exercises.Behaviors.Dynamics.Unique (
    uniqueExercise
  ) where

import Control.Monad.Fix (MonadFix)

import Reflex

uniqueExercise :: (Reflex t, MonadFix m, MonadHold t m)
               => Dynamic t (Int, Int)
               -> m (Dynamic t Int, Dynamic t Int)
uniqueExercise dIn = do
  dFst <- holdUniqDyn (fst <$> dIn)
  dSnd <- holdUniqDyn (snd <$> dIn)
  pure (dFst, dSnd)
