{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module EffWeb.Html.Attribute.Global
  ( GlobalAttribute(..)


  , HtmlId(..)
  , CssClass(..)
  , CssStyle(..)
  ) where

import           Data.Constraint.Extras
import           Data.Constraint.Extras.TH (deriveArgDict)
import qualified Data.Dependent.Map as DMap
import           Data.Dependent.Sum (DSum, (==>))
import           Data.Functor.Identity (Identity(..))
import           Data.GADT.Compare
import           Data.GADT.Compare.TH (deriveGEq,deriveGCompare)
import           Data.GADT.Show
import           Data.GADT.Show.TH (deriveGShow)
import           Data.Kind (Type)
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           EffWeb.DOM.FFI.Raw (HasSetAttributeValue(..))
import           EffWeb.Html.Attribute.Common
import           EffWeb.Html.Element (HtmlElement)
import           GHC.TypeLits

--------------------------------------------------------------------------------


newtype HtmlId = HtmlId Text
  deriving stock (Show,Eq,Ord)
  deriving newtype (IsString, HasTextRender,HasSetAttributeValue)

newtype CssClass = CssClass Text
  deriving stock (Show,Eq,Ord)
  deriving newtype (IsString, HasTextRender,HasSetAttributeValue)

newtype CssStyle = CssStyle Text
  deriving stock (Show,Eq,Ord)
  deriving newtype (IsString, HasTextRender,HasSetAttributeValue)


--------------------------------------------------------------------------------

data GlobalAttribute a where
  -- global attributes
  Accesskey             ::         GlobalAttribute Text
  Anchor                ::         GlobalAttribute Text
    -- Experimental Non-standard
  Autocapitalize        ::         GlobalAttribute Text
  Autocorrect           ::         GlobalAttribute Text
  Autofocus             ::         GlobalAttribute Text
  Class                 ::         GlobalAttribute CssClass
  Contenteditable       ::         GlobalAttribute Text
  XData                 :: Text -> GlobalAttribute Text
  Dir                   ::         GlobalAttribute Text
  Draggable             ::         GlobalAttribute Bool
  Enterkeyhint          ::         GlobalAttribute Text
  Exportparts           ::         GlobalAttribute Text
  Hidden                ::         GlobalAttribute Text
  Id                    ::         GlobalAttribute HtmlId
  Inert                 ::         GlobalAttribute Text
  Inputmode             ::         GlobalAttribute Text
  Is                    ::         GlobalAttribute Text
  Itemid                ::         GlobalAttribute Text
  Itemprop              ::         GlobalAttribute Text
  Itemref               ::         GlobalAttribute Text
  Itemscope             ::         GlobalAttribute Text
  Itemtype              ::         GlobalAttribute Text
  Lang                  ::         GlobalAttribute Text
  Nonce                 ::         GlobalAttribute Text
  Part                  ::         GlobalAttribute Text
  Popover               ::         GlobalAttribute Text
  Slot                  ::         GlobalAttribute Text
  Spellcheck            ::         GlobalAttribute Text
  Style                 ::         GlobalAttribute CssStyle
  Tabindex              ::         GlobalAttribute Text
  Title                 ::         GlobalAttribute Text
  Translate             ::         GlobalAttribute NoYes
  Virtualkeyboardpolicy ::         GlobalAttribute Text
  -- Experimental
  Writingsuggestions    ::         GlobalAttribute Text

deriving instance Show a => Show (GlobalAttribute a)

deriveGEq      ''GlobalAttribute
deriveGCompare ''GlobalAttribute
deriveGShow    ''GlobalAttribute
deriveArgDict  ''GlobalAttribute

-- | Get the Name of a HtmlAttribute
instance HasAttrName (GlobalAttribute a) where
  attrNameOf = \case
    Accesskey             -> "accesskey"
    Anchor                -> "anchor"
    Autocapitalize        -> "autocapitalize"
    Autocorrect           -> "autocorrect"
    Autofocus             -> "autofocus"
    Class                 -> "class"
    Contenteditable       -> "contenteditable"
    XData label           -> "data-" <> label
    Dir                   -> "dir"
    Draggable             -> "draggable"
    Enterkeyhint          -> "enterkeyhint"
    Exportparts           -> "exportparts"
    Hidden                -> "hidden"
    Id                    -> "id"
    Inert                 -> "inert"
    Inputmode             -> "inputmode"
    Is                    -> "is"
    Itemid                -> "itemid"
    Itemprop              -> "itemprop"
    Itemref               -> "itemref"
    Itemscope             -> "itemscope"
    Itemtype              -> "itemtype"
    Lang                  -> "lang"
    Nonce                 -> "nonce"
    Part                  -> "part"
    Popover               -> "popover"
    Slot                  -> "slot"
    Spellcheck            -> "spellcheck"
    Style                 -> "style"
    Tabindex              -> "tabindex"
    Title                 -> "title"
    Translate             -> "translate"
    Virtualkeyboardpolicy -> "virtualkeyboardpolicy"
    Writingsuggestions    -> "writingsuggestions"

--------------------------------------------------------------------------------
