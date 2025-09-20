module Pre
  ( Generic
  , List
  , NonEmpty
  , Text
  , IsString
  , module Prettyprinter
  , module Prelude
  , renderText
  , putPrettyLn
  ) where

import Data.List (List)
import Data.List.NonEmpty (NonEmpty)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.IO qualified as Text
import GHC.Generics (Generic)
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import Prelude hiding (Word)

renderText :: (Pretty a) => a -> Text
renderText = renderStrict . layoutPretty defaultLayoutOptions . pretty

putPrettyLn :: (Pretty a) => a -> IO ()
putPrettyLn = Text.putStrLn . renderText
