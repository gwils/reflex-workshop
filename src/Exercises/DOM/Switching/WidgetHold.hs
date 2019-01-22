{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module Exercises.DOM.Switching.WidgetHold (
    widgetHoldExercise
  ) where

import Data.Bool

import Reflex.Dom.Core

widgetHoldExercise :: MonadWidget t m
                   => Event t Bool
                   -> m (Dynamic t Int)
widgetHoldExercise eClickable = do
  let
    resetBtn, addBtn :: MonadWidget t m => m (Event t (Int-> Int))
    resetBtn = (const 0 <$) <$> button "Wait..."
    addBtn   = ((+1)    <$) <$> button "Click me"
    emeAlter  = bool resetBtn addBtn <$> eClickable

  deAlter <- el "div" $
    widgetHold resetBtn emeAlter
  
  let eAlter = switchDyn deAlter
  
  foldDyn id 0 eAlter
