{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Exercises.Behaviors.Creating.Apply (
    applyExercise
  ) where

import Reflex

applyExercise :: (Reflex t, MonadHold t m)
              => Event t Int
              -> m (Behavior t Int, Behavior t Int)
applyExercise eIn =
  let
    e2 = (`div` 2) <$> ffilter (\x -> x `mod` 2 == 0) eIn
    e3 = (`div` 3) <$> ffilter (\x -> x `mod` 3 == 0) eIn
  in do
    b2 <- hold 0 e2
    b3 <- hold 0 e3
    pure (b2,b3)
