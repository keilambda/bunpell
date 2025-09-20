module Pre
  ( Generic
  , List
  , NonEmpty
  , Text
  , IsString
  , module Prelude
  ) where

import Data.List (List)
import Data.List.NonEmpty (NonEmpty)
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (Word)
