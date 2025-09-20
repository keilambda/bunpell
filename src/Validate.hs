module Validate
  ( SentenceError (..)
  , hasVerb
  , validate
  ) where

import Data.Validation (Validation (..))
import Lib
import Pre

data SentenceError
  = MissingVerb
  | MultipleTopics
  | InvalidTopicParticle Particle
  deriving stock (Show)

type M a = Validation (NonEmpty SentenceError) a

hasVerb :: List Role -> M ()
hasVerb rs = case [() | Verb _ <- rs] of
  [] -> Failure (pure MissingVerb)
  _ : _ -> Success ()

hasValidTopic :: List Role -> M ()
hasValidTopic rs = case [p | Topic _ p <- rs] of
  [p] -> case p of
    Ha; Ga -> Success ()
    _ -> Failure (pure (InvalidTopicParticle p))
  _ -> Failure (pure MultipleTopics)

validate :: Sentence -> M Sentence
validate s = (hasVerb s.content *> hasValidTopic s.content) *> Success s
