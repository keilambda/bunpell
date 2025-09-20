module Validate
  ( SentenceError (..)
  , validate
  ) where

import Data.Functor
import Data.Validation (Validation (..))
import Lib
import Pre

data SentenceError
  = InvalidTopicParticle Word Particle
  | MultipleTopics (List Role)
  | VerbMismatch Word Style Style
  | UnknownVerbForm Word
  | MissingVerb
  deriving stock (Show)

type M a = Validation (NonEmpty SentenceError) a

hasValidTopic :: List Role -> M ()
hasValidTopic rs = case [x | x@(Topic _ _) <- rs] of
  [Topic w p] -> case p of
    Ha; Ga -> Success ()
    _ -> Failure (pure (InvalidTopicParticle w p))
  ts -> Failure (pure (MultipleTopics ts))

hasValidVerb :: Sentence -> M ()
hasValidVerb s = case verbOf s.content of
  Just w -> case inferStyle w of
    Just style' -> if s.style == style' then Success () else Failure (pure (VerbMismatch w s.style style'))
    Nothing -> Failure (pure (UnknownVerbForm w))
  Nothing -> Failure (pure MissingVerb)

validate :: Sentence -> M Sentence
validate s = (hasValidTopic s.content *> hasValidVerb s) $> s
