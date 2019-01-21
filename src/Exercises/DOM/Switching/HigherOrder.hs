{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RecursiveDo#-}
module Exercises.DOM.Switching.HigherOrder (
    higherOrderExercise
  ) where

import Data.Bool

import Control.Monad.Fix (MonadFix)

import Reflex.Dom.Core

higherOrderExercise :: (Reflex t, MonadFix m, MonadHold t m)
                    => Event t Bool
                    -> Event t ()
                    -> m (Dynamic t Int)
higherOrderExercise eClickable eClick = mdo
  let
    eReset = const 0 <$ eClick
    eAdd = (+1) <$ eClick

  bPlaying <- hold False eClickable

  let
    eScore = mergeWith (.) [
        gate (not <$> bPlaying) eReset
      , gate          bPlaying  eAdd
      ]

  dOut <- holdDyn 0 (flip id <$> current dOut <@> eScore)
  pure dOut
