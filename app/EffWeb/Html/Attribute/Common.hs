{-# LANGUAGE OverloadedStrings #-}
module EffWeb.Html.Attribute.Common
  ( HasAttrName(..)
  , HasTextRender(..)

  , NoYes(..)
  ) where

import Data.Functor.Identity (Identity(..))
import Data.Text (Text)
import EffWeb.DOM.FFI.Raw (HasSetAttributeValue(..))

--------------------------------------------------------------------------------

class HasAttrName attr where
  -- | Renders the attribute name
  attrNameOf :: attr -> Text

class HasTextRender a where
  -- | Renders the attribute value
  renderAsText :: a -> Text

instance HasTextRender a => HasTextRender (Identity a) where
  renderAsText (Identity x) = renderAsText x

instance HasTextRender Text where
  renderAsText = id


--------------------------------------------------------------------------------

data NoYes = No | Yes
  deriving stock (Show,Read,Eq,Ord,Enum)

instance HasSetAttributeValue NoYes where
  js_setAttribute node attr = js_setAttribute node attr . \case
    No  -> "no" :: Text
    Yes -> "yes"
