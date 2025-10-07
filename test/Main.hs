{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Validation (Validation (..))
import Lib
import Pre
import Test.Tasty
import Test.Tasty.HUnit
import Validate

main :: IO Unit
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "bunpell"
    [ prettyTests
    , helpersTests
    , validateTests
    ]

prettyTests :: TestTree
prettyTests =
  testGroup
    "Pretty"
    [ testCase "Particle pretty" do
        assertEqual "Ha" "は" (renderText Ha)
        assertEqual "Ga" "が" (renderText Ga)
        assertEqual "Wo" "を" (renderText Wo)
        assertEqual "He" "へ" (renderText He)
        assertEqual "Ni" "に" (renderText Ni)
        assertEqual "De" "で" (renderText De)
        assertEqual "No" "の" (renderText No)
    , testCase "Role pretty" do
        assertEqual "Subject" "私の" (renderText (Subject "私" No))
        assertEqual "Topic" "名前は" (renderText (Topic "名前" Ha))
        assertEqual "Object" "ケイ" (renderText (Object "ケイ"))
        assertEqual "Verb" "です" (renderText (Verb "です"))
    , testCase "Sentence pretty (concat roles)" do
        assertEqual "concat" "私の名前はケイです" . renderText
          $ MkSentence
            { content = [Subject "私" No, Topic "名前" Ha, Object "ケイ", Verb "です"]
            , style = MkStyle Polite NonPast Positive
            }
    ]

helpersTests :: TestTree
helpersTests =
  testGroup
    "Helpers"
    [ testCase "verbOf finds first verb" case verbOf [Object "本", Verb "ある", Verb "いる"] of
        Nothing -> assertFailure "Expected a verb"
        Just w -> assertEqual "first verb" "ある" (renderText w)
    , testCase "verbOf returns Nothing when no verb" case verbOf [Subject "私" No, Object "本"] of
        Nothing -> pure ()
        Just _ -> assertFailure "Expected Nothing"
    , testCase "inferStyle covers known forms" do
        inferStyle "です" @?= Just (MkStyle Polite NonPast Positive)
        inferStyle "でした" @?= Just (MkStyle Polite Past Positive)
        inferStyle "行きます" @?= Just (MkStyle Polite NonPast Positive)
        inferStyle "行きました" @?= Just (MkStyle Polite Past Positive)
        inferStyle "行きません" @?= Just (MkStyle Polite NonPast Negative)
        inferStyle "だ" @?= Just (MkStyle Casual NonPast Positive)
        inferStyle "だった" @?= Just (MkStyle Casual Past Positive)
        inferStyle "行く" @?= Nothing
    ]

validateTests :: TestTree
validateTests =
  testGroup
    "Validate.validate"
    [ testCase "Success: polite nonpast (です) with single topic は" do
        let content = [Subject "私" No, Topic "名前" Ha, Verb "です"]
        let style = MkStyle Polite NonPast Positive
        case validate MkSentence{content, style} of
          Success _ -> pure ()
          Failure es -> assertFailure ("Unexpected failure: " <> show es)
    , testCase "Success: polite past (でした) with single topic が" do
        let content = [Topic "雨" Ga, Verb "でした"]
        let style = MkStyle Polite Past Positive
        case validate MkSentence{content, style} of
          Success _ -> pure ()
          Failure es -> assertFailure ("Unexpected failure: " <> show es)
    , testCase "Success: casual nonpast (だ)" do
        let content = [Topic "彼" Ha, Verb "だ"]
        let style = MkStyle Casual NonPast Positive
        case validate MkSentence{content, style} of
          Success _ -> pure ()
          Failure es -> assertFailure ("Unexpected failure: " <> show es)
    , testCase "Success: casual past (だった)" do
        let content = [Topic "彼" Ha, Verb "だった"]
        let style = MkStyle Casual Past Positive
        case validate MkSentence{content, style} of
          Success _ -> pure ()
          Failure es -> assertFailure ("Unexpected failure: " <> show es)
    , testCase "Error: MissingVerb (topic present)" do
        let content = [Topic "彼" Ha]
        let style = MkStyle Polite NonPast Positive
        case validate MkSentence{content, style} of
          Failure es -> case NonEmpty.head es of
            MissingVerb -> pure ()
            other -> assertFailure ("Expected MissingVerb, got: " <> show other)
          Success _ -> assertFailure "Expected failure"
    , testCase "Error: UnknownVerbForm" do
        let content = [Topic "彼" Ha, Verb "行く"]
        let style = MkStyle Casual NonPast Positive
        case validate MkSentence{content, style} of
          Failure es -> case NonEmpty.head es of
            UnknownVerbForm _ -> pure ()
            other -> assertFailure ("Expected UnknownVerbForm, got: " <> show other)
          Success _ -> assertFailure "Expected failure"
    , testCase "Error: VerbMismatch" do
        let content = [Topic "彼" Ha, Verb "です"]
        let style = MkStyle Casual NonPast Positive
        case validate MkSentence{content, style} of
          Failure es -> case NonEmpty.head es of
            VerbMismatch{} -> pure ()
            other -> assertFailure ("Expected VerbMismatch, got: " <> show other)
          Success _ -> assertFailure "Expected failure"
    , testCase "Error: InvalidTopicParticle (single topic not は/が)" do
        let content = [Topic "彼" Wo, Verb "です"]
        let style = MkStyle Polite NonPast Positive
        case validate MkSentence{content, style} of
          Failure es -> case NonEmpty.head es of
            InvalidTopicParticle{} -> pure ()
            other -> assertFailure ("Expected InvalidTopicParticle, got: " <> show other)
          Success _ -> assertFailure "Expected failure"
    , testCase "Error: MultipleTopics (two topics)" do
        let content = [Topic "彼" Ha, Topic "私" Ga, Verb "でした"]
        let style = MkStyle Polite Past Positive
        case validate MkSentence{content, style} of
          Failure es -> case NonEmpty.head es of
            MultipleTopics _ -> pure ()
            other -> assertFailure ("Expected MultipleTopics, got: " <> show other)
          Success _ -> assertFailure "Expected failure"
    , testCase "Error accumulation: MultipleTopics + VerbMismatch" do
        let content = [Topic "彼" Ha, Topic "私" Ga, Verb "でした"]
        let style = MkStyle Casual NonPast Positive
        case validate MkSentence{content, style} of
          Failure (toList -> es) -> do
            assertBool "has MultipleTopics" (any isMultipleTopics es)
            assertBool "has VerbMismatch" (any isVerbMismatch es)
          Success _ -> assertFailure "Expected failure"
    ]
 where
  isMultipleTopics = \case MultipleTopics _ -> True; _ -> False
  isVerbMismatch = \case VerbMismatch{} -> True; _ -> False
