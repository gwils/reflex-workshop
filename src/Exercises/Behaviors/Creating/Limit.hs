{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RecursiveDo #-}
module Exercises.Behaviors.Creating.Limit (
    limitExercise
  ) where

import Control.Applicative
import Control.Monad.Fix (MonadFix)
import Data.Function ((&))

import Reflex

limitExercise :: (Reflex t, MonadFix m, MonadHold t m)
             => Behavior t Int
             -> Event t ()
             -> Event t ()
             -> m (Behavior t Int)
limitExercise bCount eAdd eReset = mdo
  let
    def = 5
    bEq = liftA2 (==) bCount bLimit
    b0 = (==0) <$> bCount
    eFn = mergeWith (.) [
          (+1)    <$ gate bEq eReset
        , const def <$ gate b0 eReset
      ]
    eBonus = flip id <$> bLimit <@> eFn
  
  bBonus <- hold def eBonus
  -- let bLimit = bBonus + pure 5
  let bLimit = bBonus
  pure bLimit
