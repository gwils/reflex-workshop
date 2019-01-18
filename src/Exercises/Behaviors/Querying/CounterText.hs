{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Exercises.Behaviors.Querying.CounterText (
    counterTextExercise
  ) where

import Data.Text (Text)

import Reflex

import Exercises.Events.Filtering.FmapMaybe
import Exercises.Behaviors.Querying.Counter

counterTextExercise :: Reflex t
                    => Behavior t Int
                    -> Behavior t Text
                    -> Event t ()
                    -> Event t ()
                    -> (Event t Text, Event t Int)
counterTextExercise bCount bText eAdd eReset =
  let
    (oeErr, oeAdd) = fmapMaybeExercise (bText <@ eAdd)
  in
    (oeErr, counterExercise' bCount oeAdd eReset)
