{-# LANGUAGE OverloadedStrings #-}

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
  = Ha
  | Ga
  | Wo
  | He
  | Ni
  | De
  | No
  deriving stock (Show)

instance Pretty Particle where
  pretty = \case
    Ha -> "は"
    Ga -> "が"
    Wo -> "を"
    He -> "へ"
    Ni -> "に"
    De -> "で"
    No -> "の"

newtype Word = MkWord Text
  deriving newtype (IsString, Pretty, Show)

data Role
  = Subject Word Particle
  | Topic Word Particle
  | Object Word
  | Verb Word
  deriving stock (Show)

instance Pretty Role where
  pretty = \case
    Subject w p -> pretty w <> pretty p
    Topic w p -> pretty w <> pretty p
    Object w -> pretty w
    Verb w -> pretty w

data Sentence = MkSentence
  { content :: List Role
  , tense :: Tense
  , formality :: Formality
  }
  deriving stock (Show)

instance Pretty Sentence where
  pretty s = hcat (pretty <$> s.content)
