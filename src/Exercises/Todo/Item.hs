{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
module Exercises.Todo.Item where

import Control.Monad
import Data.Bool
import Data.Monoid

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

import Reflex.Dom.Core

import Common.Todo

todoText :: MonadWidget t m
         => Dynamic t Bool
         -> Text
         -> m (Event t (Text -> Text), Event t ())
todoText dComplete iText = mdo
  let
    dCompleteClass = bool "" " completed" <$> dComplete

    bValue = current (ti ^. textInput_value)
    eEnter = keypress Enter ti
    splitOnEmpty t = if Text.null t then Right () else Left (const t)

  ti <- elDynClass "div" (pure "p-1" <> dCompleteClass) $
      textInput $ def & textInputConfig_initialValue .~ iText
  pure $ fanEither $ splitOnEmpty <$> bValue <@ eEnter

todoItem :: MonadWidget t m
         => TodoItem
         -> m (Event t (TodoItem -> TodoItem), Event t ())
todoItem item = divClass "d-flex flex-row align-items-center" $ mdo
  let
    iText = item ^. todoItem_text
    iComplete = item ^. todoItem_complete
  
  eComplete <- todoCheckbox iComplete
  dComplete <- foldDyn id iComplete eComplete

  (eText, eNoText) <- todoText dComplete iText

  btn <- todoRemove

  let
    fn = set todoItem_complete <$> leftmost [updated dComplete, True <$ eText]
    eRemove = leftmost [btn, eNoText]
  pure (fn, eRemove)

todoItemExercise :: MonadWidget t m
                 => TodoItem
                 -> m ()
todoItemExercise ti = do
  (eChange,eWidget) <- todoItem ti
  countDisplay "Change" eChange
  countDisplay "Remove" eWidget

countDisplay :: MonadWidget t m => Text -> Event t a -> m ()
countDisplay name dIn = divClass "p-1" $ do
  dCount <- count dIn
  text name
  text " has been fired "
  display dCount
  text " time"
  dynText $ ffor dCount $ \c -> if c /= 1 then "s" else ""

todoCheckbox :: MonadWidget t m => Bool -> m (Event t (Bool -> Bool))
todoCheckbox b = divClass "p-1" $ do
  cb <- checkbox b def
  pure (const <$> cb ^. checkbox_change)

todoRemove :: MonadWidget t m => m (Event t ())
todoRemove =
  divClass "p-1" $
    button "x"
