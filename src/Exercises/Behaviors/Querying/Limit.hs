{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Exercises.Behaviors.Querying.Limit (
    limitExercise
  ) where

import Control.Applicative (liftA2)
import Reflex

import Exercises.Behaviors.Querying.Counter

limitExercise :: Reflex t
              => Behavior t Int
              -> Behavior t Int
              -> Event t ()
              -> Event t ()
              -> Event t Int
limitExercise bCount bLimit eAdd eReset =
  counterExercise bCount eAdd' eReset
    where
      eAdd' = gate (liftA2 (<) bCount bLimit) eAdd
