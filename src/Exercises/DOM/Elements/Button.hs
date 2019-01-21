{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module Exercises.DOM.Elements.Button (
    buttonExercise
  ) where

import Reflex.Dom.Core

import Util.Bootstrap

buttonExercise :: MonadWidget t m
               => m ()
buttonExercise = do
  add <- el "div" $ button "Add"
  res <- el "div" $ button "Reset"
  txt <- foldDyn id 0 $ mergeWith (.) [
      (+1) <$ add
    , const 0 <$ res
    ]
  el "div" $ display txt
  pure ()
