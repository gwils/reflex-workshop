{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Exercises.DOM.Switching.Workflow (
    workflowExercise
  ) where

import Reflex.Dom.Core

workflowExercise :: forall t m . MonadWidget t m
                 => Event t ()
                 -> m (Dynamic t Int)
workflowExercise eChange = do
  let
    resetBtn, addBtn :: Workflow t m (Event t (Int-> Int))
    resetBtn = Workflow $ do
      eResult <- (const 0 <$) <$> button "Wait..."
      pure (eResult, addBtn <$ eChange)

    addBtn   = Workflow $ do
      eResult <- ((+1)    <$) <$> button "Click me"
      pure (eResult, resetBtn <$ eChange)

  deAlter <- el "div" $
    workflow resetBtn
  
  let eAlter = switchDyn deAlter
  
  foldDyn id 0 eAlter
