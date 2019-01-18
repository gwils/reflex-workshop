{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module Exercises.Events.Combining.Leftmost (
    leftmostExercise
  ) where

import Data.Monoid

import Data.Text (Text)
import qualified Data.Text as Text

import Reflex

leftmostExercise :: Reflex t
                 => Event t Int
                 -> ( Event t Text
                    , Event t Text
                    , Event t Text
                    , Event t Text
                    )
leftmostExercise eIn =
  (fizz, buzz, fizzbuzz, out)
    where
      fizzbuzz = fizz <> buzz
      fizz = "Fizz" <$ ffilter (\x -> x `mod` 3 == 0) eIn
      buzz = "Buzz" <$ ffilter (\x -> x `mod` 5 == 0) eIn
      out = leftmost [fizzbuzz, Text.pack . show <$> eIn]
