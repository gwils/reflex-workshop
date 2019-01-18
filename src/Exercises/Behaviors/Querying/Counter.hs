{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Exercises.Behaviors.Querying.Counter (
    counterExercise'
  , counterExercise
  ) where

import Reflex

counterExercise' :: Reflex t
                 => Behavior t Int
                 -> Event t Int
                 -> Event t ()
                 -> Event t Int
counterExercise' bCount eAdd eReset =
  leftmost [
    attachWith (+) bCount eAdd
  , 0 <$ bCount <@ eReset
  ]

counterExercise :: Reflex t
                => Behavior t Int
                -> Event t ()
                -> Event t ()
                -> Event t Int
counterExercise bCount eAdd eReset =
  counterExercise' bCount (1 <$ eAdd) eReset
