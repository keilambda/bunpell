module Lib
  ( Politeness (..)
  , Tense (..)
  , Particle (..)
  ) where

data Politeness
  = Casual
  | Formal

data Tense
  = Past
  | NonPast

data Particle
  = Wa -- は
  | Ga -- が
  | Wo -- を
  | He -- へ
  | Ni -- に
  | De -- で
  | No -- の
