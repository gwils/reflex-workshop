{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
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
higherOrderExercise eClickable eClick = do
  let
    eReset = const 0 <$ eClick
    eAdd   = (+ 1)   <$ eClick
    eeAlter = bool eReset eAdd <$> eClickable

  eAlter <-  switchHold never eeAlter
  foldDyn id 0 eAlter
