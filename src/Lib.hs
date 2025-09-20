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
  deriving newtype (Show, IsString)

data Role
  = Subject Word Particle
  | Topic Word Particle
  | Object Word
  | Verb Word
  deriving stock (Show)

data Sentence = MkSentence
  { content :: List Role
  , tense :: Tense
  , politeness :: Politeness
  }
  deriving stock (Show)
