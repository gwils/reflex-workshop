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
    eInsert = (\(k,v) -> k =: Just v) <$> eAdd
    m = Map.fromList . zip [0..] $ items
  dmes <- listHoldWithKey m eListChange $ \_ item -> do
    (eChange, eRemove) <- todoItem item
    let eItem = ($ item) <$> eChange
    pure (eItem, eRemove)

  let
    eRemoves =
      fmap (Nothing <$) .
      switchDyn .
      fmap (mergeMap . fmap snd) $
      dmes
    eListChange =
      leftmost [eRemoves, eInsert]
    eMapOut = switchDyn $ fmap (mergeMap . fmap fst) dmes
    eListOut = Map.elems <$> eMapOut
  dListOut <- holdDyn [] eListOut

  pure dListOut
