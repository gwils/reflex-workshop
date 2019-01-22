{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
module Exercises.Todo.List where

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

import Reflex.Dom.Core

import Common.Todo
import Exercises.Todo.Item

addItem :: MonadWidget t m => m (Event t TodoItem)
addItem =
  el "div" $ mdo
    ti <- textInput $ def & textInputConfig_setValue .~ ("" <$ eValue)
    let
      dValue = ti ^. textInput_value
      dStripped = Text.strip <$> dValue
      eEnter = keypress Enter ti
      eValue = ffilter (not . Text.null) (current dValue <@ eEnter)
    pure (TodoItem False <$> eValue)


todoListExercise :: MonadWidget t m
                 => [TodoItem]
                 -> m ()
todoListExercise items = mdo
  eAdd <- addItem
  eAdd' <- numberOccurrencesFrom (length items) (Just <$> eAdd)
  let
    eRemove' = fmap (,Nothing) eRemove
    eAddRemove = leftmost [eAdd', eRemove']
    m = Map.fromList . zip [0..] $ items
    eNewMap = ffor eAddRemove $ \(n,mti) -> n =: mti
  dMap <- listHoldWithKey m eNewMap $ \_ item ->
    todoItem item
  let eRemove = switchDyn $ fmap (leftmost . fmap (uncurry (<$)) . Map.toList . fmap snd) dMap
  pure ()

todoListModelExercise :: MonadWidget t m
                      => [TodoItem]
                      -> m (Dynamic t [TodoItem])
todoListModelExercise items =
  pure (pure [])
