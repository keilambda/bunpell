{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( Formality (..)
  , Politeness (..)
  , Tense (..)
  , Mood (..)
  , Verb (..)
  , Adjective (..)
  , Style (..)
  , Particle (..)
  , Word (..)
  , Role (..)
  , Sentence (..)
  , verbOf
  , conjugateVerb
  , conjugateAdjective
  , inferStyle
  ) where

import Data.Text (isSuffixOf, stripSuffix)
import Pre

data Formality
  = Casual
  | Formal
  deriving stock (Eq, Show)

data Politeness
  = Plain
  | Polite
  deriving stock (Eq, Show)

data Tense
  = Past
  | NonPast
  deriving stock (Eq, Show)

data Mood
  = Positive
  | Negative
  deriving stock (Eq, Show)

data Verb
  = Ichidan Word
  | Godan Word

data Adjective
  = I Word
  | Na Word

data Style = MkStyle
  { formality :: Formality
  , politeness :: Politeness
  , tense :: Tense
  , mood :: Mood
  }
  deriving stock (Eq, Show)

instance Pretty Style where
  pretty s = braces $ pretty (show s.formality) <> comma <+> pretty (show s.tense) <> comma <+> pretty (show s.mood)

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

conjugateVerb :: Style -> Verb -> Maybe Verb
conjugateVerb s = \case
  Ichidan (MkWord w) -> do
    stem <- stripSuffix "る" w
    pure . Ichidan $ MkWord case s.politeness of
      Plain -> case s.tense of
        Past -> case s.mood of
          Positive -> stem <> "た"
          Negative -> stem <> "なかった"
        NonPast -> case s.mood of
          Positive -> w
          Negative -> stem <> "ない"
      Polite -> case s.tense of
        Past -> case s.mood of
          Positive -> stem <> "ました"
          Negative -> stem <> "ませんでした"
        NonPast -> case s.mood of
          Positive -> stem <> "ます"
          Negative -> stem <> "ません"
  Godan (MkWord _w) -> do
    undefined

conjugateAdjective :: Style -> Adjective -> Maybe Adjective
conjugateAdjective s = \case
  I (MkWord w) -> do
    stem <- if w == "いい" then pure "よ" else stripSuffix "い" w
    pure . I $ MkWord case s.politeness of
      Plain -> case s.tense of
        Past -> case s.mood of
          Positive -> stem <> "かった"
          Negative -> stem <> "くなかった"
        NonPast -> case s.mood of
          Positive -> w
          Negative -> stem <> "くない"
      Polite -> case s.tense of
        Past -> case s.mood of
          Positive -> stem <> "かったです"
          Negative -> case s.formality of
            Casual -> stem <> "くなかったです"
            Formal -> stem <> "くありませんでした"
        NonPast -> case s.mood of
          Positive -> w <> "です"
          Negative -> case s.formality of
            Casual -> stem <> "くないです"
            Formal -> stem <> "くありません"
  Na (MkWord w) ->
    pure . Na $ MkWord case s.politeness of
      Plain -> case s.tense of
        Past -> case s.mood of
          Positive -> w <> "だった"
          Negative -> case s.formality of
            Casual -> w <> "じゃなかった"
            Formal -> w <> "ではなかった"
        NonPast -> case s.mood of
          Positive -> w <> "だ"
          Negative -> case s.formality of
            Casual -> w <> "じゃない"
            Formal -> w <> "ではない"
      Polite -> case s.tense of
        Past -> case s.mood of
          Positive -> w <> "でした"
          Negative -> case s.formality of
            Casual -> w <> "じゃなかったです"
            Formal -> w <> "ではありませんでした"
        NonPast -> case s.mood of
          Positive -> w <> "です"
          Negative -> case s.formality of
            Casual -> w <> "じゃないです"
            Formal -> w <> "ではありません"

inferStyle :: Word -> Maybe Style
inferStyle (MkWord t) =
  if
    | suf "です" || suf "ます" -> Just (MkStyle Formal Polite NonPast Positive)
    | suf "でした" || suf "ました" -> Just (MkStyle Formal Polite Past Positive)
    | suf "ません" -> Just (MkStyle Formal Polite NonPast Negative)
    | suf "だ" -> Just (MkStyle Casual Plain NonPast Positive)
    | suf "だった" -> Just (MkStyle Casual Plain Past Positive)
    | otherwise -> Nothing
 where
  suf s = s `isSuffixOf` t
