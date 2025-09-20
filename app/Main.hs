{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Validation (Validation (..))
import Lib
import Pre
import Validate

main :: IO ()
main = case validate me of
  Failure fs -> print fs
  Success s -> putPrettyLn s

me :: Sentence
me = MkSentence{content, style = MkStyle{tense = NonPast, formality = Formal}}
 where
  content =
    [ Subject "私" No
    , Topic "名前" Ha
    , Object "ケイ"
    , Verb "です"
    ]
