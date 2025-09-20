{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib
import Validate

main :: IO ()
main = print (validate me)

me :: Sentence
me = MkSentence{content, tense = NonPast, politeness = Formal}
 where
  content =
    [ Subject "私" No
    , Topic "名前" Ha
    , Object "ケイ"
    , Verb "です"
    ]
