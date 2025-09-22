{-# LANGUAGE OverloadedStrings #-}

module Validate
  ( SentenceError (..)
  , validate
  ) where

import Data.Functor
import Data.Proxy
import Data.Text qualified as Text
import Data.Validation (Validation (..))
import GHC.Records
import GHC.Tuple
import GHC.TypeLits
import Lib
import Pre

data SentenceError
  = InvalidTopicParticle Word Particle
  | MultipleTopics (List Role)
  | VerbMismatch Word Style Style
  | UnknownVerbForm Word
  | MissingVerb
  deriving stock (Show)

instance Pretty SentenceError where
  pretty = \case
    InvalidTopicParticle w p -> "Invalid topic particle" <> colon <+> dquotes (pretty w) <+> dquotes (pretty p)
    MultipleTopics rs -> "Multiple topics" <> colon <+> hsep (pretty <$> rs)
    VerbMismatch w expected found -> do
      let (expDoc, foundDoc) = diffStyle expected found
      ("Verb mismatch" <> colon)
        <+> dquotes (pretty w)
        <+> ("expected=" <> braces (hsep (punctuate comma expDoc)))
        <+> ("found=" <> braces (hsep (punctuate comma foundDoc)))
    UnknownVerbForm w -> "Unknown verb form" <> colon <+> dquotes (pretty w)
    MissingVerb -> "Missing a verb"
   where
    diffStyle expected found = ([kv k v | (k, v, _) <- diffs], [kv k v | (k, _, v) <- diffs])
     where
      diffs = mconcat [diff (Proxy @"formality"), diff (Proxy @"tense"), diff (Proxy @"mood")]

      diff
        :: forall field a ann
         . (Eq a, HasField field Style a, KnownSymbol field, Show a)
        => Proxy field
        -> List (Tuple3 Text (Doc ann) (Doc ann))
      diff proxy =
        let expected' = getField @field @Style expected
            found' = getField @field @Style found
         in [ (Text.pack (symbolVal proxy), pretty (show expected'), pretty (show found'))
            | expected' /= found'
            ]

      kv k v = pretty k <> "=" <> v

type M a = Validation (NonEmpty SentenceError) a

hasValidTopic :: List Role -> M Unit
hasValidTopic rs = case [x | x@(Topic _ _) <- rs] of
  [Topic w p] -> case p of
    Ha; Ga -> Success ()
    _ -> Failure (pure (InvalidTopicParticle w p))
  ts -> Failure (pure (MultipleTopics ts))

hasValidVerb :: Sentence -> M Unit
hasValidVerb s = case verbOf s.content of
  Just w -> case inferStyle w of
    Just style' -> if s.style == style' then Success () else Failure (pure (VerbMismatch w s.style style'))
    Nothing -> Failure (pure (UnknownVerbForm w))
  Nothing -> Failure (pure MissingVerb)

validate :: Sentence -> M Sentence
validate s = (hasValidTopic s.content *> hasValidVerb s) $> s
