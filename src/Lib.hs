{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( Politeness (..)
  , Tense (..)
  , Particle (..)
  , Word (..)
  , Role (..)
  , Sentence (..)
  ) where

import Data.List
import Data.String (IsString)
import Pre

data Politeness
  = Casual
  | Formal

data Tense
  = Past
  | NonPast

data Particle
  = Ha -- は
  | Ga -- が
  | Wo -- を
  | He -- へ
  | Ni -- に
  | De -- で
  | No -- の

newtype Word = MkWord Text
  deriving newtype (IsString)

data Role
  = Subject Word Particle
  | Topic Word Particle
  | Object Word
  | Verb Word

data Sentence = MkSentence
  { content :: List Role
  , tense :: Tense
  , politeness :: Politeness
  }

foo :: Sentence
foo = MkSentence{content, tense = NonPast, politeness = Formal}
 where
  content =
    [ Subject "私" No
    , Topic "名前" Ha
    , Object "ケイ"
    , Verb "です"
    ]
