module Pre
  ( Generic
  , List
  , Unit
  , Tuple3
  , NonEmpty
  , Text
  , IsString
  , module Data.Foldable
  , module Prettyprinter
  , module Safe
  , module Prelude
  , renderText
  , putPrettyLn
  ) where

import Data.Foldable
import Data.List (List)
import Data.List.NonEmpty (NonEmpty)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.IO qualified as Text
import GHC.Generics (Generic)
import GHC.Tuple (Tuple3, Unit)
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import Safe
import Prelude hiding (Word)

renderText :: (Pretty a) => a -> Text
renderText = renderStrict . layoutPretty defaultLayoutOptions . pretty

putPrettyLn :: (Pretty a) => a -> IO Unit
putPrettyLn = Text.putStrLn . renderText
