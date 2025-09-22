{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Validation (Validation (..))
import Lib
import Pre
import Validate

main :: IO Unit
main = case validate me of
  Failure fs -> traverse_ putPrettyLn fs
  Success s -> putPrettyLn s

me :: Sentence
me = MkSentence{content, style = MkStyle{tense = NonPast, formality = Formal, mood = Positive}}
 where
  content =
    [ Subject "私" No
    , Topic "名前" Ha
    , Object "ケイ"
    , Verb "です"
    ]
