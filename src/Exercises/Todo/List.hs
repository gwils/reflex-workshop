{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Exercises.Todo.List where

import Control.Lens
import Control.Monad

import qualified Data.Text as Text

import qualified Data.Map as Map

import Reflex.Dom.Core

import Common.Todo
import Exercises.Todo.Item

addItem :: MonadWidget t m
        => m (Event t TodoItem)
addItem = mdo
  ti <- textInput $
    def & setValue .~ ("" <$ eEnter)

  let
    eEnter    = keypress Enter ti
    bText     = current $ value ti
    eStripped = Text.strip <$> bText <@ eEnter
    eText     = ffilter (not . Text.null) eStripped
    eAdd      = TodoItem False <$> eText

  pure eAdd

todoListExercise :: MonadWidget t m
                 => [TodoItem]
                 -> m ()
todoListExercise = void . todoListModelExercise

todoListModelExercise :: MonadWidget t m
                      => [TodoItem]
                      -> m (Dynamic t [TodoItem])
todoListModelExercise items = mdo
  eItem <- addItem
  eAdd  <- numberOccurrencesFrom (length items) eItem

  let
    eInsert = ffor eAdd $ \(k,v) -> k =: Just v
    eListChange = leftmost [eRemoves, eInsert]
    m = Map.fromList . zip [0..] $ items

  (dmd, eRemoves) <- runEventWriterT . listHoldWithKey m eListChange $ \k item -> mdo
    (eChange, eRemove) <- todoItem item
    tellEvent ((k =: Nothing) <$ eRemove)
    foldDyn id item eChange

  pure (Map.elems <$> joinDynThroughMap dmd)
