module Lib
  ( Formality (..)
  , Tense (..)
  , Particle (..)
  , Word (..)
  , Role (..)
  , Sentence (..)
  ) where

import Pre

data Formality
  = Casual
  | Formal
  deriving stock (Show)

data Tense
  = Past
  | NonPast
  deriving stock (Show)

data Particle
  = Ha -- は
  | Ga -- が
  | Wo -- を
  | He -- へ
  | Ni -- に
  | De -- で
  | No -- の
  deriving stock (Show)

newtype Word = MkWord Text
  deriving newtype (IsString, Show)

data Role
  = Subject Word Particle
  | Topic Word Particle
  | Object Word
  | Verb Word
  deriving stock (Show)

data Sentence = MkSentence
  { content :: List Role
  , tense :: Tense
  , formality :: Formality
  }
  deriving stock (Show)
