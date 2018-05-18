{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module Workshop.Miscellany.Servant (
    servantSubSection
  ) where

import Reflex.Dom.Core

import Util.Exercises
import Util.Section

servantSubSection :: MonadWidget t m => SubSection t m
servantSubSection =
  SubSection "Servant" .
  runProblems "../pages/miscellany/servant.md" $
  []
