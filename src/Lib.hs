{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( Formality (..)
  , Tense (..)
  , Mood (..)
  , Style (..)
  , Particle (..)
  , Word (..)
  , Role (..)
  , Sentence (..)
  , verbOf
  , inferStyle
  ) where

import Data.Text (isSuffixOf)
import Pre

data Formality
  = Casual
  | Formal
  deriving stock (Eq, Show)

data Tense
  = Past
  | NonPast
  deriving stock (Eq, Show)

data Mood
  = Positive
  | Negative
  deriving stock (Eq, Show)

data Style = MkStyle
  { formality :: Formality
  , tense :: Tense
  , mood :: Mood
  }
  deriving stock (Eq, Show)

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
  , style :: Style
  }
  deriving stock (Show)

instance Pretty Sentence where
  pretty s = hcat (pretty <$> s.content)

verbOf :: List Role -> Maybe Word
verbOf rs = headMay [w | Verb w <- rs]

inferStyle :: Word -> Maybe Style
inferStyle (MkWord t) =
  if
    | suf "です" || suf "ます" -> Just (MkStyle Formal NonPast Positive)
    | suf "でした" || suf "ました" -> Just (MkStyle Formal Past Positive)
    | suf "ません" -> Just (MkStyle Formal NonPast Negative)
    | suf "だ" -> Just (MkStyle Casual NonPast Positive)
    | suf "だった" -> Just (MkStyle Casual Past Positive)
    | otherwise -> Nothing
 where
  suf s = s `isSuffixOf` t
