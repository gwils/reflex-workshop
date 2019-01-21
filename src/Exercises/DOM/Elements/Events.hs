{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module Exercises.DOM.Elements.Events (
    eventsExercise
  ) where

import Data.Bool
import Data.Monoid

import Reflex.Dom.Core

eventsExercise :: MonadWidget t m
               => Dynamic t Bool
               -> m a
               -> m (Event t ())
eventsExercise dIn w =
  let
    hidden = fmap (bool mempty ("hidden" =: "")) dIn
  in do
    (e,_) <- elDynAttr' "div" (pure ("class" =: "text-uppercase") <> hidden) w
    pure (domEvent Click e)
