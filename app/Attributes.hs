module        Attributes where

import qualified Data.Dependent.Map as DMap
import           Data.Kind (Type)
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           HtmlElement (HtmlElement)

--------------------------------------------------------------------------------
-- | Maybe used on all elements
data GlobalAttribute = Accesskey
                     | Anchor -- Experimental Non-standard
                     | Autocapitalize
                     | Autocorrect
                     | Autofocus
                     | Class
                     | Contenteditable
                     | XData Text
                     | Dir
                     | Draggable
                     | Enterkeyhint
                     | Exportparts
                     | Hidden
                     | Id
                     | Inert
                     | Inputmode
                     | Is
                     | Itemid
                     | Itemprop
                     | Itemref
                     | Itemscope
                     | Itemtype
                     | Lang
                     | Nonce
                     | Part
                     | Popover
                     | Slot
                     | Spellcheck
                     | Style
                     | Tabindex
                     | Title
                     | Translate
                     | Virtualkeyboardpolicy -- Experimental
                     | Writingsuggestions
                     deriving (Show,Eq,Ord)


newtype HtmlId = HtmlId Text
  deriving stock (Show,Eq,Ord)
  deriving newtype (IsString)

--------------------------------------------------------------------------------

data AttributeKind = Global GlobalAttribute
                   | Specific HtmlElement
  deriving (Show,Eq,Ord)

type family AttributeValue (attr :: AttributeKind) :: Type

type instance AttributeValue (Global Id)    = HtmlId
type instance AttributeValue (Global Title) = Text
